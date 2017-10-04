package argon.lang.direct

import argon.core._
import forge._

trait HashMapApi {
  implicit class ArrayGroupByOps[A](array: MArray[A]) {
    private implicit val mA: Type[A] = array.s.tp.typeArguments.head.asInstanceOf[Type[A]]
    /** Partitions this Array using the `key` function, then maps each element using `value`, and
      * finally combines values in each bin using the associative `reduce` function.
      */
    @api def groupByReduce[K:Type,V:Type](key: A => K)(value: A => V)(reduce: (V,V) => V): MHashMap[K,V] = {
      val i = fresh[Index]
      val rV = (fresh[V],fresh[V])
      val (keys, values, index) = {
        MHashMap.build_hashmap(array.s, {a:Exp[A] => key(wrap(a)).s}, {a:Exp[A] => value(wrap(a)).s}, {(a:Exp[V],b:Exp[V]) => reduce(wrap(a),wrap(b)).s}, rV, i)
      }
      wrap(MHashMap.hashmap_new(keys, values, index, wrap(keys).length.s))
    }
  }
}