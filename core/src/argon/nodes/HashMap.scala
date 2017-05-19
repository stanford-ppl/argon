package argon.nodes

import argon.compiler._

case class HashIndexType[K](mK: Type[K]) extends Type[HashIndex[K]] {
  override def wrapped(x: Exp[HashIndex[K]]) = new HashIndex(x)(mK)
  override def stagedClass = classOf[HashIndex[K]]
  override def typeArguments = List(mK)
  override def isPrimitive = true
}

case class HashMapType[K,V](mK: Type[K], mV: Type[V]) extends StructType[MHashMap[K,V]] {
  override def wrapped(x: Exp[MHashMap[K,V]]) = MHashMap(x)(mK,mV)
  override def stagedClass = classOf[MHashMap[K,V]]
  override def typeArguments = List(mK, mV)
  override def isPrimitive = true
  override def fields = Seq("keys" -> ArrayType(mK), "values" -> ArrayType(mV), "index" -> HashIndexType(mK), "size" -> IntType)
}

/** IR Nodes **/
// Gets an integer key entry from a hash index, -1 if it is not present
case class HashIndexApply[K:Type](index: Exp[HashIndex[K]], key: Exp[K]) extends Op[Index] {
  def mirror(f:Tx) = MHashMap.hash_index_apply(f(index), f(key))
}

// Creates a struct representing an Argon Map with fields keys, values, index, and size
case class HashMapNew[K:Type,V:Type](
  keys:   Exp[MArray[K]],
  values: Exp[MArray[V]],
  index:  Exp[HashIndex[K]],
  size:   Exp[Index]
) extends StructAlloc[MHashMap[K,V]] {
  def elems = Seq("keys" -> keys, "values" -> values, "index" -> index, "size" -> size)
  def mirror(f:Tx) = MHashMap.hashmap_new(f(keys),f(values),f(index),f(size))
}

// Creates an array of keys from an initial data structure
// TODO: Should be a subclass of groupByReduce, and probably multiple nodes to begin with
// For now just experimenting with creating (fat) Defs early in IR
case class BuildHashMap[A:Type,K:Type,V:Type](
  in:      Exp[MArray[A]],
  apply:   Lambda2[MArray[A],Index,A],
  keyFunc: Lambda1[A,K],
  valFunc: Lambda1[A,V],
  reduce:  Lambda2[V,V,V],
  rV:      (Bound[V],Bound[V]),
  i:       Bound[Index]
) extends Def {
  def fatMirror(f:Tx) = {
    val out = MHashMap.build_hashmap(f(in),f(keyFunc),f(valFunc),f(reduce),rV,i)
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