package argon.lang

import argon._
import forge._

case class HashIndex[K:Type](s: Exp[HashIndex[K]]) extends MetaAny[HashIndex[K]] {
  @api def =!=(x: HashIndex[K]): Bool = ??? // TODO! but never seen by user currently
  @api def ===(x: HashIndex[K]): Bool = ??? // TODO! but never seen by user currently
  @api def toText: Text = Text.ify(this)
}

case class HashMap[K:Type,V:Type](s: Exp[HashMap[K,V]]) extends Struct[HashMap[K,V]] {
  @api def keys: Array[K]   = field[Array[K]]("keys")
  @api def values: Array[V] = field[Array[V]]("values")
  @api def size: Index      = field[Index]("size")

  @internal private def index = field[HashIndex[K]]("index")
  @internal private def getIndex(key: K): Index = wrap(HashMap.hash_index_apply(this.index.s, key.s))

  @api def apply(key: K): V = this.values.apply(this.getIndex(key))
  @api def contains(key: K): Bool = this.getIndex(key) =!= -1
}

case class HashIndexType[K](mK: Type[K]) extends Type[HashIndex[K]] {
  override def wrapped(x: Exp[HashIndex[K]]) = HashIndex(x)(mK)
  override def stagedClass = classOf[HashIndex[K]]
  override def typeArguments = List(mK)
  override def isPrimitive = true
}

case class HashMapType[K,V](mK: Type[K], mV: Type[V]) extends StructType[HashMap[K,V]] {
  override def wrapped(x: Exp[HashMap[K,V]]) = HashMap(x)(mK,mV)
  override def stagedClass = classOf[HashMap[K,V]]
  override def typeArguments = List(mK, mV)
  override def isPrimitive = true
  override def fields = Seq("keys" -> ArrayType(mK), "values" -> ArrayType(mV), "index" -> HashIndexType(mK), "size" -> IntType)
}

object HashMap {
  /** Constructors **/
  @internal def hash_index_apply[K:Type](index: Exp[HashIndex[K]], key: Exp[K]): Exp[Index] = {
    stage( HashIndexApply(index, key) )(ctx)
  }

  @internal def hashmap_new[K:Type,V:Type](
    keys:   Exp[Array[K]],
    values: Exp[Array[V]],
    index:  Exp[HashIndex[K]],
    size:   Exp[Index]
  ): Exp[HashMap[K,V]] = {
    stage( HashMapNew(keys,values,index,size) )(ctx)
  }

  @internal def build_hashmap[A:Type,K:Type,V:Type](
    in:      Exp[Array[A]],
    keyFunc: Exp[A] => Exp[K],
    valFunc: Exp[A] => Exp[V],
    reduce:  (Exp[V],Exp[V]) => Exp[V],
    rV:      (Bound[V],Bound[V]),
    i:       Bound[Index]
  ): (Exp[Array[K]], Exp[Array[V]], Exp[HashIndex[K]]) = {
    val aBlk = stageLambda(in, i) { Array.apply(in, i) }
    val kBlk = stageLambda(aBlk.result){ keyFunc(aBlk.result) }
    val vBlk = stageLambda(aBlk.result){ valFunc(aBlk.result) }
    val rBlk = stageLambda(rV._1,rV._2) { reduce(rV._1,rV._2) }
    val effects = aBlk.effects andAlso kBlk.effects andAlso vBlk.effects andAlso rBlk.effects
    val out = stageDefEffectful( BuildHashMap(in, aBlk, kBlk, vBlk, rBlk, rV, i), effects.star)(ctx)

    val keys   = out(0).asInstanceOf[Exp[Array[K]]]
    val values = out(1).asInstanceOf[Exp[Array[V]]]
    val index  = out(2).asInstanceOf[Exp[HashIndex[K]]]
    (keys, values, index)
  }
}

trait HashMapExp {
  /** Type classes **/
  implicit def hashIndexIsStaged[K:Type]: Type[HashIndex[K]] = HashIndexType(typ[K])
  implicit def hashMapIsStaged[K:Type,V:Type]: StructType[HashMap[K,V]] = HashMapType(typ[K],typ[V])
}

trait HashMapApi {
  implicit class ArrayGroupByOps[A](array: Array[A]) {
    private implicit val mA: Type[A] = array.s.tp.typeArguments.head.asInstanceOf[Type[A]]
    @api def groupByReduce[K:Type,V:Type](key: A => K)(value: A => V)(reduce: (V,V) => V): HashMap[K,V] = {
      val i = fresh[Index]
      val rV = (fresh[V],fresh[V])
      val (keys, values, index) = {
        HashMap.build_hashmap(array.s, {a => key(wrap(a)).s}, {a => value(wrap(a)).s}, {(a,b) => reduce(wrap(a),wrap(b)).s}, rV, i)
      }
      wrap(HashMap.hashmap_new(keys, values, index, wrap(keys).length.s))
    }
  }
}



/** IR Nodes **/
// Gets an integer key entry from a hash index, -1 if it is not present
case class HashIndexApply[K:Type](index: Exp[HashIndex[K]], key: Exp[K]) extends Op[Index] {
  def mirror(f:Tx) = HashMap.hash_index_apply(f(index), f(key))
}

// Creates a struct representing an Argon Map with fields keys, values, index, and size
case class HashMapNew[K:Type,V:Type](
  keys:   Exp[Array[K]],
  values: Exp[Array[V]],
  index:  Exp[HashIndex[K]],
  size:   Exp[Index]
) extends StructAlloc[HashMap[K,V]] {
  def elems = Seq("keys" -> keys, "values" -> values, "index" -> index, "size" -> size)
  def mirror(f:Tx) = HashMap.hashmap_new(f(keys),f(values),f(index),f(size))
}

// Creates an array of keys from an initial data structure
// TODO: Should be a subclass of groupByReduce, and probably multiple nodes to begin with
// For now just experimenting with creating (fat) Defs early in IR
case class BuildHashMap[A:Type,K:Type,V:Type](
  in:      Exp[Array[A]],
  apply:   Lambda2[Array[A],Index,A],
  keyFunc: Lambda1[A,K],
  valFunc: Lambda1[A,V],
  reduce:  Lambda2[V,V,V],
  rV:      (Bound[V],Bound[V]),
  i:       Bound[Index]
) extends Def {
  def fatMirror(f:Tx) = {
    val out = HashMap.build_hashmap(f(in),f(keyFunc),f(valFunc),f(reduce),rV,i)
    List(out._1,out._2,out._3)
  }

  def outputTypes = List(ArrayType(typ[K]), ArrayType(typ[V]), HashIndexType(typ[K]))

  override def inputs = dyns(in) ++ dyns(apply) ++ dyns(keyFunc) ++ dyns(valFunc) ++ dyns(reduce)
  override def freqs = normal(in) ++ hot(apply) ++ hot(keyFunc) ++ hot(valFunc) ++ hot(reduce)
  override def aliases = Nil
  override def binds = dyns(rV._1, rV._2, i)

  val mA: Type[A] = typ[A]
  val mK: Type[K] = typ[K]
  val mV: Type[V] = typ[V]
}