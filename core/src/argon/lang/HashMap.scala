package argon.lang

import argon.compiler._
import argon.nodes._
import forge._

case class HashIndex[K:Type](s: Exp[HashIndex[K]]) extends MetaAny[HashIndex[K]] {
  val mK: MetaAny[K] = typ[K].fake
  override type Internal = scala.collection.immutable.HashMap[mK.Internal,scala.Int]

  @api def =!=(x: HashIndex[K]): MBoolean = ??? // TODO! but never seen by user currently
  @api def ===(x: HashIndex[K]): MBoolean = ??? // TODO! but never seen by user currently
  @api def toText: MString = String.ify(this)
}

case class HashMap[K:Type,V:Type](s: Exp[HashMap[K,V]]) extends Struct[HashMap[K,V]] {
  val mK: MetaAny[K] = typ[K].fake
  val mV: MetaAny[V] = typ[V].fake
  override type Internal = scala.collection.immutable.HashMap[mK.Internal,mV.Internal]

  @api def keys: Array[K]   = field[Array[K]]("keys")
  @api def values: Array[V] = field[Array[V]]("values")
  @api def size: Index      = field[Index]("size")

  @internal private def index = field[HashIndex[K]]("index")
  @internal private def getIndex(key: K): Index = wrap(HashMap.hash_index_apply(this.index.s, key.s))

  @api def apply(key: K): V = this.values.apply(this.getIndex(key))
  @api def contains(key: K): MBoolean = this.getIndex(key) =!= -1
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
    val aBlk = stageLambda2(in, i) { Array.apply(in, i) }
    val kBlk = stageLambda1(aBlk.result){ keyFunc(aBlk.result) }
    val vBlk = stageLambda1(aBlk.result){ valFunc(aBlk.result) }
    val rBlk = stageLambda2(rV._1,rV._2) { reduce(rV._1,rV._2) }
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
        HashMap.build_hashmap(array.s, {a:Exp[A] => key(wrap(a)).s}, {a:Exp[A] => value(wrap(a)).s}, {(a:Exp[V],b:Exp[V]) => reduce(wrap(a),wrap(b)).s}, rV, i)
      }
      wrap(HashMap.hashmap_new(keys, values, index, wrap(keys).length.s))
    }
  }
}


