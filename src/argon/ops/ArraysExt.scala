package argon.ops

/**
  * Array operations are separated into two categories. ArrayOps is for use in all DSLs which allow at least the use
  * of command-line arguments. ArrayExtOps is for more complicated operations, including update and parallel patterns.
  */
trait ArrayExtApi extends ArrayExtExp with ArrayApi {
  object Array {
    def tabulate[T:Meta](size: Index)(func: Index => T)(implicit ctx: SrcCtx): MetaArray[T] = array_from_function[T](size, func)
    def fill[T:Meta](size: Index)(func: => T)(implicit ctx: SrcCtx): MetaArray[T] = Array.tabulate(size){_ => func}
    def empty[T:Meta](size: Index)(implicit ctx: SrcCtx): MetaArray[T] = MetaArray(array_new[T](size.s))
  }

  implicit class ArrayInfixOps[T:Meta](a: MetaArray[T]) {
    def update[A](i: Index, data: A)(implicit ctx: SrcCtx, lift: Lift[A,T]): Void = array_infix_update(a, i, lift(data))
    def foreach(func: T => Void)(implicit ctx: SrcCtx): Void = array_infix_foreach(a, func)
    def map[R:Meta](func: T => R)(implicit ctx: SrcCtx): MetaArray[R] = array_infix_map(a, func)
    def zip[S:Meta,R:Meta](b: MetaArray[S])(func: (T,S) => R)(implicit ctx: SrcCtx): MetaArray[R] = array_infix_zip(a, b, func)
    def reduce(rfunc: (T,T) => T)(implicit ctx: SrcCtx): T = array_infix_reduce(a, rfunc)
    def filter(cond: T => Bool)(implicit ctx: SrcCtx): MetaArray[T] = array_infix_filter(a, cond)
    def flatMap[R:Meta](func: T => MetaArray[R])(implicit ctx: SrcCtx): MetaArray[R] = array_infix_flatMap(a, func)
  }

  implicit class NestedArrayInfixOps[T:Meta](a: MetaArray[MetaArray[T]]) {
    def flatten(implicit ctx: SrcCtx): MetaArray[T] = ArrayInfixOps(a).flatMap{x => x}
  }
}

trait ArrayExtExp extends ArrayExp {

}
