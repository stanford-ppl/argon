package argon.ops

import argon._
import forge._

trait HashMapApi extends HashMapExp { self: ArgonApi =>
  type HashMap[K,V] = MetaHashMap[K,V]

  implicit class ArrayGroupByOps[A](array: MetaArray[A]) {
    @api def groupByReduce[K:Meta,V:Meta](key: A => K)(value: A => V)(reduce: (V,V) => V)(implicit ctx: SrcCtx, mA: Meta[A]): MetaHashMap[K,V] = {
      val i = fresh[Index]
      val rV = (fresh[V],fresh[V])
      val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s }
      val kBlk = stageLambda(aBlk.result){ key(wrap(aBlk.result)).s }
      val vBlk = stageLambda(aBlk.result){ value(wrap(aBlk.result)).s }
      val rBlk = stageBlock { reduce(wrap(rV._1),wrap(rV._2)).s }
      val effects = aBlk.summary andAlso kBlk.summary andAlso vBlk.summary andAlso rBlk.summary

      val out = stageDefEffectful( BuildHashMap(array.s, aBlk, kBlk, vBlk, rBlk, rV, i), effects.star)(ctx)
      val keys   = out(0).asInstanceOf[Exp[MetaArray[K]]]
      val values = out(1).asInstanceOf[Exp[MetaArray[V]]]
      val index  = out(2).asInstanceOf[Exp[HashIndex[K]]]
      wrap(hashmap_new(keys, values, index, wrap(keys).length.s))
    }
  }
}

trait HashMapExp { self: ArgonExp =>
  /** Infix methods **/
  case class HashIndex[K:Meta](s: Exp[HashIndex[K]]) extends MetaAny[HashIndex[K]] {
    @api def =!=(x: HashIndex[K]): Bool = ??? // TODO! but never seen by user currently
    @api def ===(x: HashIndex[K]): Bool = ??? // TODO! but never seen by user currently
    @api def toText: Text = textify(this)
  }

  case class MetaHashMap[K:Meta,V:Meta](s: Exp[MetaHashMap[K,V]]) extends MetaStruct[MetaHashMap[K,V]] {
    @api def keys: MetaArray[K]   = field[MetaArray[K]]("keys")
    @api def values: MetaArray[V] = field[MetaArray[V]]("values")
    @api def size: Index          = field[Index]("size")

    private def index(implicit ctx: SrcCtx) = field[HashIndex[K]]("index")
    private def getIndex(key: K)(implicit ctx: SrcCtx): Index = wrap(hash_index_apply(this.index.s, key.s))

    @api def apply(key: K): V = this.values.apply(this.getIndex(key))
    @api def contains(key: K): Bool = this.getIndex(key) =!= -1
  }

  /** Type classes **/
  // --- Staged
  case class HashIndexType[K](mK: Meta[K]) extends Meta[HashIndex[K]] {
    override def wrapped(x: Exp[HashIndex[K]]) = HashIndex(x)(mK)
    override def stagedClass = classOf[HashIndex[K]]
    override def typeArguments = List(mK)
    override def isPrimitive = true
  }
  implicit def stagedHash[K:Meta]: Type[HashIndex[K]] = HashIndexType(meta[K])

  case class HashMapType[K,V](mK: Meta[K], mV: Meta[V]) extends StructType[MetaHashMap[K,V]] {
    override def wrapped(x: Exp[MetaHashMap[K,V]]) = MetaHashMap(x)(mK,mV)
    override def stagedClass = classOf[MetaHashMap[K,V]]
    override def typeArguments = List(mK, mV)
    override def isPrimitive = true
    override def fields = Seq("keys" -> ArrayType(mK), "values" -> ArrayType(mV), "index" -> HashIndexType(mK), "size" -> IntType)
  }
  implicit def stagedMap[K:Meta,V:Meta]: StructType[MetaHashMap[K,V]] = HashMapType(meta[K],meta[V])

  /** IR Nodes **/
  // Gets an integer key entry from a hash index, -1 if it is not present
  case class HashIndexApply[K:Type](index: Exp[HashIndex[K]], key: Exp[K]) extends Op[Index] {
    def mirror(f:Tx) = hash_index_apply(f(index), f(key))
  }

  // Creates a struct representing an Argon Map with fields keys, values, index, and size
  case class HashMapNew[K:Type,V:Type](
    keys:   Exp[MetaArray[K]],
    values: Exp[MetaArray[V]],
    index:  Exp[HashIndex[K]],
    size:   Exp[Index]
  ) extends StructAlloc[MetaHashMap[K,V]] {
    def elems = Seq("keys" -> keys, "values" -> values, "index" -> index, "size" -> size)
    def mirror(f:Tx) = hashmap_new(f(keys),f(values),f(index),f(size))
  }

  // Creates an array of keys from an initial data structure
  // TODO: Should be a subclass of groupByReduce, and probably multiple nodes to begin with
  // For now just experimenting with creating (fat) Defs early in IR
  case class BuildHashMap[A:Meta,K:Meta,V:Meta](
    in:      Exp[MetaArray[A]],
    apply:   Block[A],
    keyFunc: Block[K],
    valFunc: Block[V],
    reduce:  Block[V],
    rV:      (Bound[V],Bound[V]),
    i:       Bound[Index]
  ) extends Def {
    def fatMirror(f:Tx) = {
      val out = build_hashmap(f(in),f(apply),f(keyFunc),f(valFunc),f(reduce),rV,i)
      List(out._1,out._2,out._3)
    }

    def outputTypes = List(ArrayType(meta[K]), ArrayType(meta[V]), HashIndexType(meta[K]))

    override def inputs = dyns(in) ++ dyns(apply) ++ dyns(keyFunc) ++ dyns(valFunc) ++ dyns(reduce)
    override def freqs = normal(in) ++ hot(apply) ++ hot(keyFunc) ++ hot(valFunc) ++ hot(reduce)
    override def aliases = Nil
    override def binds = dyns(rV._1, rV._2, i)

    val mA: Meta[A] = meta[A]
    val mK: Meta[K] = meta[K]
    val mV: Meta[V] = meta[V]
  }


  /** Constructors **/
  protected def hash_index_apply[K:Type](index: Exp[HashIndex[K]], key: Exp[K])(implicit ctx: SrcCtx): Exp[Index] = {
    stage( HashIndexApply(index, key) )(ctx)
  }

  protected def hashmap_new[K:Type,V:Type](
    keys:   Exp[MetaArray[K]],
    values: Exp[MetaArray[V]],
    index:  Exp[HashIndex[K]],
    size:   Exp[Index]
  )(implicit ctx: SrcCtx): Exp[MetaHashMap[K,V]] = {
    stage( HashMapNew(keys,values,index,size) )(ctx)
  }

  private def build_hashmap[A:Meta,K:Meta,V:Meta](
    in:      Exp[MetaArray[A]],
    apply:   => Exp[A],
    keyFunc: => Exp[K],
    valFunc: => Exp[V],
    reduce:  => Exp[V],
    rV:      (Bound[V],Bound[V]),
    i:       Bound[Index]
  )(implicit ctx: SrcCtx): (Exp[MetaArray[K]], Exp[MetaArray[V]], Exp[HashIndex[K]]) = {
    val aBlk = stageLambda(in) { apply }
    val kBlk = stageLambda(aBlk.result){ keyFunc }
    val vBlk = stageLambda(aBlk.result){ valFunc }
    val rBlk = stageBlock { reduce }
    val effects = aBlk.summary andAlso kBlk.summary andAlso vBlk.summary andAlso rBlk.summary
    val out = stageDefEffectful( BuildHashMap(in, aBlk, kBlk, vBlk, rBlk, rV, i), effects.star)(ctx)

    val keys   = out(0).asInstanceOf[Exp[MetaArray[K]]]
    val values = out(1).asInstanceOf[Exp[MetaArray[V]]]
    val index  = out(2).asInstanceOf[Exp[HashIndex[K]]]
    (keys, values, index)
  }

}
