package argon.ops

import argon._
import forge._

/**
  * Array operations are separated into two categories. ArrayOps is for use in all DSLs which allow at least the use
  * of command-line arguments. ArrayExtOps is for more complicated operations, including update and parallel patterns.
  */
trait ArrayExtApi extends ArrayExtExp with ArrayApi { self: ArgonApi =>
  object Array {
    @api def tabulate[T:Meta](size: Index)(func: Index => T): MetaArray[T] = array_from_function[T](size, func)
    @api def fill[T:Meta](size: Index)(func: => T): MetaArray[T] = Array.tabulate(size){i => func}
    @api def empty[T:Meta](size: Index): MetaArray[T] = MetaArray(array_new[T](size.s))
    @api def const[T:Meta](elements: T*): MetaArray[T] = {
      val arr = empty(elements.length)
      (0 until elements.length).foreach{ i => 
        arr(i) = elements(i)
        ()
      }
      arr
    }
  }

  implicit class ArrayInfixOps[T](a: MetaArray[T]) {
    // Fun hack to avoid chaining implicits when resolving this implicit class
    private implicit val mT = a.s.tp.typeArguments.head.asInstanceOf[Meta[T]]
    @api def update[A](i: Index, data: A)(implicit lift: Lift[A,T]): Void = array_infix_update(a, i, lift(data))
    @api def foreach(func: T => Void): Void = array_infix_foreach(a, func)
    @api def map[R:Meta](func: T => R): MetaArray[R] = array_infix_map(a, func)
    @api def zip[S:Meta,R:Meta](b: MetaArray[S])(func: (T,S) => R): MetaArray[R] = array_infix_zip(a, b, func)
    @api def reduce(rfunc: (T,T) => T): T = array_infix_reduce(a, rfunc)
    @api def filter(cond: T => Bool): MetaArray[T] = array_infix_filter(a, cond)
    @api def flatMap[R:Meta](func: T => MetaArray[R]): MetaArray[R] = array_infix_flatMap(a, func)
  }

  implicit class NestedArrayInfixOps[T](a: MetaArray[MetaArray[T]]) {
    private implicit val mT = a.s.tp.typeArguments.head.typeArguments.head.asInstanceOf[Meta[T]]
    @api def flatten: MetaArray[T] = ArrayInfixOps(a).flatMap{x => x}
  }
}

trait ArrayExtExp extends ArrayExp { self: ArgonExp => }
