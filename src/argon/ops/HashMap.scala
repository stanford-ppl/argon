package argon.ops

import argon.core.Staging
import org.virtualized.virtualize

trait HashMapApi extends HashMapExp with ArrayApi with StructApi {

  implicit class ArrayGroupByOps[A:Staged](array: Array[A]) {
    def groupByReduce[K:Staged,V:Staged](key: A => K)(value: A => V)(reduce: (V,V) => V)(implicit ctx: SrcCtx): ArgonMap[K,V] = {
      val i = fresh[Index]
      val rV = (fresh[V],fresh[V])
      val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s : Exp[A] }
      val kBlk = stageLambda(aBlk.result){ key(wrap(aBlk.result)).s }
      val vBlk = stageLambda(aBlk.result){ value(wrap(aBlk.result)).s }
      val rBlk = stageBlock { reduce(wrap(rV._1),wrap(rV._2)).s }
      val effects = aBlk.summary andAlso kBlk.summary andAlso vBlk.summary andAlso rBlk.summary

      val out = stageDefEffectful( ArgonBuildHashMap(array.s, aBlk, kBlk, vBlk, rBlk, rV, i), effects.star)(ctx)
      val keys   = out(0).asInstanceOf[Exp[ArgonArray[K]]]
      val values = out(1).asInstanceOf[Exp[ArgonArray[V]]]
      val index  = out(2).asInstanceOf[Exp[HashIndex[K]]]
      wrap(argon_map_new(keys, values, index, wrap(keys).length.s))
    }
  }
}

trait HashMapExp extends Staging with ArrayExp with StructExp {
  /** Infix methods **/
  case class HashIndex[K:Staged](s: Exp[HashIndex[K]])

  case class ArgonMap[K:Staged,V:Staged](s: Exp[ArgonMap[K,V]]) extends Struct[ArgonMap[K,V]] {
    def keys(implicit ctx: SrcCtx): ArgonArray[K]   = field[ArgonArray[K]]("keys")
    def values(implicit ctx: SrcCtx): ArgonArray[V] = field[ArgonArray[V]]("values")
    def size(implicit ctx: SrcCtx): Index           = field[Index]("size")

    private def index(implicit ctx: SrcCtx) = field[HashIndex[K]]("index")
    private def get(key: K)(implicit ctx: SrcCtx): Index = wrap(hash_index_apply(this.index.s, key.s))
    def apply(key: K)(implicit ctx: SrcCtx): V = this.values.apply(this.get(key))
    @virtualize
    def contains(key: K)(implicit ctx: SrcCtx): Bool = this.get(key) != lift(-1)
  }

  /** Type classes **/
  // --- Staged
  case class HashIndexType[K](mK: Staged[K]) extends Staged[HashIndex[K]] {
    override def wrapped(x: Exp[HashIndex[K]]) = HashIndex(x)(mK)
    override def unwrapped(x: HashIndex[K]) = x.s
    override def stagedClass = classOf[HashIndex[K]]
    override def typeArguments = List(mK)
    override def isPrimitive = true
  }
  implicit def stagedHash[K:Staged]: Staged[HashIndex[K]] = HashIndexType(typ[K])

  case class ArgonMapType[K,V](mK: Staged[K], mV: Staged[V]) extends StructType[ArgonMap[K,V]] {
    override def wrapped(x: Exp[ArgonMap[K,V]]) = ArgonMap(x)(mK,mV)
    override def unwrapped(x: ArgonMap[K,V]) = x.s
    override def stagedClass = classOf[ArgonMap[K,V]]
    override def typeArguments = List(mK, mV)
    override def isPrimitive = true
    override def fields = Seq("keys" -> ArrayType(mK), "values" -> ArrayType(mV), "index" -> HashIndexType(mK), "size" -> IntType)
  }
  implicit def stagedMap[K:Staged,V:Staged]: StructType[ArgonMap[K,V]] = ArgonMapType(typ[K],typ[V])


  /** IR Nodes **/
  // Gets an integer key entry from a hash index, -1 if it is not present
  case class HashIndexApply[K:Staged](index: Exp[HashIndex[K]], key: Exp[K]) extends Op[Index] {
    def mirror(f:Tx) = hash_index_apply(f(index), f(key))
  }

  // Creates a struct representing an Argon Map with fields keys, values, index, and size
  case class ArgonMapNew[K:Staged,V:Staged](
    keys:   Exp[ArgonArray[K]],
    values: Exp[ArgonArray[V]],
    index:  Exp[HashIndex[K]],
    size:   Exp[Index]
  ) extends StructAlloc[ArgonMap[K,V]] {
    def elems = Seq("keys" -> keys, "values" -> values, "index" -> index, "size" -> size)
    def mirror(f:Tx) = argon_map_new(f(keys),f(values),f(index),f(size))
  }

  // Creates an array of keys from an initial data structure
  // TODO: Should be a subclass of groupByReduce, and probably multiple nodes to begin with
  // For now just experimenting with creating (fat) Defs early in IR
  case class ArgonBuildHashMap[A:Staged,K:Staged,V:Staged](
    in:      Exp[ArgonArray[A]],
    apply:   Block[A],
    keyFunc: Block[K],
    valFunc: Block[V],
    reduce:  Block[V],
    rV:      (Bound[V],Bound[V]),
    i:       Bound[Index]
  ) extends Def {
    def fatMirror(f:Tx) = {
      val out = argon_build_hashmap(f(in),f(apply),f(keyFunc),f(valFunc),f(reduce),rV,i)
      List(out._1,out._2,out._3)
    }

    def outputTypes = List(ArrayType(typ[K]), ArrayType(typ[V]), HashIndexType(typ[K]))

    override def inputs = syms(in) ++ syms(apply) ++ syms(keyFunc) ++ syms(valFunc) ++ syms(reduce)
    override def freqs = normal(in) ++ hot(apply) ++ hot(keyFunc) ++ hot(valFunc) ++ hot(reduce)
    override def aliases = Nil
    override def binds = syms(rV._1, rV._2, i)

    val mA = typ[A]
    val mK = typ[K]
    val mV = typ[V]
  }


  /** Constructors **/
  protected def hash_index_apply[K:Staged](index: Exp[HashIndex[K]], key: Exp[K])(implicit ctx: SrcCtx): Exp[Index] = {
    stage( HashIndexApply(index, key) )(ctx)
  }

  protected def argon_map_new[K:Staged,V:Staged](
    keys:   Exp[ArgonArray[K]],
    values: Exp[ArgonArray[V]],
    index:  Exp[HashIndex[K]],
    size:   Exp[Index]
  )(implicit ctx: SrcCtx): Exp[ArgonMap[K,V]] = {
    stage( ArgonMapNew(keys,values,index,size) )(ctx)
  }

  private def argon_build_hashmap[A:Staged,K:Staged,V:Staged](
    in:      Exp[ArgonArray[A]],
    apply:   => Exp[A],
    keyFunc: => Exp[K],
    valFunc: => Exp[V],
    reduce:  => Exp[V],
    rV:      (Bound[V],Bound[V]),
    i:       Bound[Index]
  )(implicit ctx: SrcCtx): (Exp[ArgonArray[K]], Exp[ArgonArray[V]], Exp[HashIndex[K]]) = {
    val aBlk = stageLambda(in) { apply }
    val kBlk = stageLambda(aBlk.result){ keyFunc }
    val vBlk = stageLambda(aBlk.result){ valFunc }
    val rBlk = stageBlock { reduce }
    val effects = aBlk.summary andAlso kBlk.summary andAlso vBlk.summary andAlso rBlk.summary
    val out = stageDefEffectful( ArgonBuildHashMap(in, aBlk, kBlk, vBlk, rBlk, rV, i), effects.star)(ctx)

    val keys   = out(0).asInstanceOf[Exp[ArgonArray[K]]]
    val values = out(1).asInstanceOf[Exp[ArgonArray[V]]]
    val index  = out(2).asInstanceOf[Exp[HashIndex[K]]]
    (keys, values, index)
  }

}
