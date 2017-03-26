package argon.ops

import org.virtualized.stageany

/**
  * Array operations are separated into two categories. ArrayOps is for use in all DSLs which allow at least the use
  * of command-line arguments. ArrayExtOps is for more complicated operations, including update and parallel patterns.
  */
@stageany
trait ArrayExtApi extends ArrayExtExp with ArrayApi {
  object Array {
    def tabulate[T:StageAny](size: Index)(func: Index => T)(implicit ctx: SrcCtx): Array[T] = array_from_function[T](size, func)
    def fill[T:StageAny](size: Index)(func: => T)(implicit ctx: SrcCtx): Array[T] = Array.tabulate(size){i => func}
    def empty[T:StageAny](size: Index)(implicit ctx: SrcCtx): Array[T] = Array[T](size)
  }

  implicit class ArrayInfixOps[T:StageAny](a: Array[T]) {
    def update[A](i: Index, data: T)(implicit ctx: SrcCtx): Void = array_infix_update(a, i, data)
    def foreach(func: T => Void)(implicit ctx: SrcCtx): Void = array_infix_foreach(a, func)
    def map[R:StageAny](func: T => R)(implicit ctx: SrcCtx): Array[R] = array_infix_map(a, func)
    def zip[S:StageAny,R:StageAny](b: Array[S])(func: (T,S) => R)(implicit ctx: SrcCtx): Array[R] = array_infix_zip(a, b, func)
    def reduce(rfunc: (T,T) => T)(implicit ctx: SrcCtx): T = array_infix_reduce(a, rfunc)
    def filter(cond: T => Bool)(implicit ctx: SrcCtx): Array[T] = array_infix_filter(a, cond)
    def flatMap[R:StageAny](func: T => Array[R])(implicit ctx: SrcCtx): Array[R] = array_infix_flatMap(a, func)
  }

  implicit class NestedArrayInfixOps[T:StageAny](a: Array[Array[T]]) {
    def flatten(implicit ctx: SrcCtx): Array[T] = a.flatMap{x => x}
  }
}

@stageany
trait ArrayExtExp extends ArrayExp {
  private[argon] def array_infix_update[T:StageAny](array: ArgonArray[T], i: Index, data: T)(implicit ctx: SrcCtx): Void = {
    Void(array_update(array.s, i.s, data.s))
  }
  private[argon] def array_from_function[T:StageAny](size: Index, func: Index => T)(implicit ctx: SrcCtx): ArgonArray[T] = {
    val i = fresh[Index]
    val fBlk = Fun0(func(wrap(i)).s)
    ArgonArray( array_mapindices(size.s, fBlk(), i) )
  }
  private[argon] def array_infix_foreach[T:StageAny](array: ArgonArray[T], func: T => Void)(implicit ctx: SrcCtx): Void = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result){ func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    Void( stageEffectful(ArrayForeach(array.s, aBlk, fBlk, i), effects.star)(ctx) )
  }
  private[argon] def array_infix_map[T:StageAny ,R <: StageAny[R] : FStaged](array: ArgonArray[T], func: T => R)(implicit ctx: SrcCtx): ArgonArray[R] = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result) { func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayMap(array.s, aBlk, fBlk, i), effects.star)(ctx)
    ArgonArray(out)
  }
  private[argon] def array_infix_zip[T:StageAny,S:StageAny,R:StageAny](a: ArgonArray[T], b: ArgonArray[S], func: (T,S) => R)(implicit ctx: SrcCtx): ArgonArray[R] = {
    val i = fresh[Index]
    val aBlk = stageBlock { a.apply(wrap(i)).s }
    val bBlk = stageBlock { b.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func(wrap(aBlk.result),wrap(bBlk.result)).s }
    val effects = aBlk.summary andAlso bBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayZip(a.s,b.s,aBlk,bBlk,fBlk,i), effects.star)(ctx)
    ArgonArray(out)
  }
  private[argon] def array_infix_reduce[T:StageAny](array: ArgonArray[T], reduce: (T,T) => T)(implicit ctx: SrcCtx): T = {
    val i = fresh[Index]
    val rV = (fresh[T],fresh[T])
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val rBlk = stageBlock { reduce(wrap(rV._1),wrap(rV._2)).s }
    val effects = aBlk.summary andAlso rBlk.summary
    val out = stageEffectful(ArrayReduce(array.s,aBlk,rBlk,i,rV),effects.star)(ctx)
    wrap(out)
  }
  private[argon] def array_infix_filter[T:StageAny](array: ArgonArray[T], filter: T => Bool)(implicit ctx: SrcCtx): ArgonArray[T] = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val cBlk = stageLambda(aBlk.result) { filter(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso cBlk.summary
    val out = stageEffectful(ArrayFilter(array.s,aBlk,cBlk,i), effects.star)(ctx)
    ArgonArray(out)
  }
  private[argon] def array_infix_flatMap[T:StageAny,R:StageAny](array: ArgonArray[T], func: T => ArgonArray[R])(implicit ctx: SrcCtx): ArgonArray[R] = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result){ func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayFlatMap(array.s,aBlk,fBlk,i), effects.star)(ctx)
    ArgonArray(out)
  }

  /** IR Nodes **/
  case class ArrayUpdate[T:StageAny](array: Exp[ArgonArray[T]], i: Exp[Int32], e: Exp[T]) extends Op[Void] {
    def mirror(f:Tx) = array_update(f(array),f(i),f(e))
    override def contains = syms(e)
  }
  case class MapIndices[T:StageAny](size: Exp[Index], func: Block[T], i: Bound[Index]) extends Op[ArgonArray[T]] {
    def mirror(f:Tx) = array_mapindices(f(size),f(func),i)
    override def inputs = syms(size) ++ syms(func)
    override def freqs  = normal(size) ++ hot(func)
    override def binds = i +: super.binds

    override def aliases = Nil

    val mT = ftyp[T]
  }
  case class ArrayForeach[T:StageAny](
    array: Exp[ArgonArray[T]],
    apply: Block[T],
    func:  Block[Void],
    i:     Bound[Index]
  ) extends Op[Void] {
    def mirror(f:Tx) = array_foreach(f(array),f(apply),f(func),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = i +: super.binds
    override def tunnels = syms(array)
    val mT = ftyp[T]
  }
  case class ArrayMap[T:StageAny, S:StageAny](
    array: Exp[ArgonArray[T]],
    apply: Block[T],
    func:  Block[S],
    i: Bound[Index]
  ) extends Op[ArgonArray[S]] {
    def mirror(f:Tx) = array_map(f(array),f(apply),f(func),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = i +: super.binds
    override def tunnels = syms(array)

    override def aliases = Nil

    val mT = ftyp[T]
    val mS = ftyp[S]
  }

  case class ArrayZip[A:StageAny,B:StageAny,C:StageAny](
    arrayA: Exp[ArgonArray[A]],
    arrayB: Exp[ArgonArray[B]],
    applyA: Block[A],
    applyB: Block[B],
    func:   Block[C],
    i:      Bound[Index]
  ) extends Op[ArgonArray[C]] {
    def mirror(f:Tx) = array_zip(f(arrayA),f(arrayB),f(applyA),f(applyB),f(func),i)
    override def inputs = syms(arrayA) ++ syms(arrayB) ++ syms(applyA) ++ syms(applyB) ++ syms(func)
    override def freqs  = normal(arrayA) ++ normal(arrayB) ++ hot(applyA) ++ hot(applyB) ++ hot(func)
    override def binds = i +: super.binds
    override def tunnels = syms(arrayA) ++ syms(arrayB)

    override def aliases = Nil

    val mA = ftyp[A]
    val mB = ftyp[B]
    val mC = ftyp[C]
  }

  case class ArrayReduce[A:StageAny](
    array:  Exp[ArgonArray[A]],
    apply:  Block[A],
    reduce: Block[A],
    i:      Bound[Index],
    rV:     (Bound[A],Bound[A])
  ) extends Op[A] {
    def mirror(f:Tx) = array_reduce(f(array),f(apply),f(reduce),i,rV)
    override def inputs = syms(array) ++ syms(apply) ++ syms(reduce)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(reduce)
    override def binds = super.binds ++ Seq(i, rV._1, rV._2)
    override def tunnels = syms(array)

    val mA = ftyp[A]
  }

  case class ArrayFilter[A:StageAny](
    array: Exp[ArgonArray[A]],
    apply: Block[A],
    cond:  Block[Bool],
    i:     Bound[Index]
  ) extends Op[ArgonArray[A]] {
    def mirror(f:Tx) = array_filter(f(array),f(apply),f(cond),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(cond)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(cond)
    override def binds = i +: super.binds
    override def tunnels = syms(array)

    override def aliases = Nil
  }

  case class ArrayFlatMap[A:StageAny,B:StageAny](
    array: Exp[ArgonArray[A]],
    apply: Block[A],
    func:  Block[ArgonArray[B]],
    i:     Bound[Index]
  ) extends Op[ArgonArray[B]] {
    def mirror(f:Tx) = array_flatmap(f(array),f(apply),f(func),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = i +: super.binds
    override def tunnels = syms(array)

    override def aliases = Nil
  }


  /** Constructors **/
  def array_update[T:StageAny](array: Exp[ArgonArray[T]], i: Exp[Int32], e: Exp[T])(implicit ctx: SrcCtx): Sym[Void] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }

  def array_mapindices[T:StageAny](
    size: Exp[Index],
    func: => Exp[T],
    i: Bound[Index]
  )(implicit ctx: SrcCtx): Sym[ArgonArray[T]] = {
    val blk = stageBlock{ func }
    val effects = blk.summary
    stageEffectful(MapIndices(size, blk, i), effects.star)(ctx)
  }

  def array_foreach[T:StageAny](
    array: Exp[ArgonArray[T]],
    apply: => Exp[T],
    func: => Exp[Void],
    i: Bound[Index]
  )(implicit ctx: SrcCtx): Sym[Void] = {
    val aBlk = stageBlock{ apply }
    val fBlk = stageLambda(aBlk.result){ func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayForeach(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  def array_map[T:StageAny, R <: StageAny[R] : FStaged](
    array: Exp[ArgonArray[T]],
    apply: => Exp[T],
    func: => Exp[R],
    i: Bound[Index]
  )(implicit ctx: SrcCtx): Sym[ArgonArray[R]] = {
    val aBlk = stageBlock { apply }
    val fBlk = stageLambda(aBlk.result) { func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayMap(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  def array_zip[A:StageAny, B <: StageAny[B] : FStaged, C <: StageAny[C] : FStaged](
    a: Exp[ArgonArray[A]],
    b: Exp[ArgonArray[B]],
    applyA: => Exp[A],
    applyB: => Exp[B],
    func:   => Exp[C],
    i:      Bound[Index]
  )(implicit ctx: SrcCtx): Sym[ArgonArray[C]] = {
    val aBlk = stageBlock { applyA }
    val bBlk = stageBlock { applyB }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func }
    val effects = aBlk.summary andAlso bBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayZip(a, b, aBlk, bBlk, fBlk, i), effects.star)(ctx)
  }

  def array_reduce[A:StageAny](
    array:  Exp[ArgonArray[A]],
    apply:  => Exp[A],
    reduce: => Exp[A],
    i:      Bound[Index],
    rV:     (Bound[A],Bound[A])
  )(implicit ctx: SrcCtx): Sym[A] = {
    val aBlk = stageBlock { apply }
    val rBlk = stageLambda(aBlk.result) { reduce }
    val effects = aBlk.summary andAlso rBlk.summary
    stageEffectful(ArrayReduce(array,aBlk,rBlk,i,rV), effects.star)(ctx)
  }

  def array_filter[A:StageAny](
    array: Exp[ArgonArray[A]],
    apply: => Exp[A],
    cond:  => Exp[Bool],
    i:     Bound[Index]
  )(implicit ctx: SrcCtx): Sym[ArgonArray[A]] = {
    val aBlk = stageBlock { apply }
    val cBlk = stageLambda(aBlk.result) { cond }
    val effects = aBlk.summary andAlso cBlk.summary
    stageEffectful(ArrayFilter(array,aBlk,cBlk,i), effects.star)(ctx)
  }

  def array_flatmap[A:StageAny,B:StageAny](
    array: Exp[ArgonArray[A]],
    apply: => Exp[A],
    func:  => Exp[ArgonArray[B]],
    i:     Bound[Index]
  )(implicit ctx: SrcCtx): Sym[ArgonArray[B]] = {
    val aBlk = stageBlock { apply }
    val fBlk = stageLambda(aBlk.result) { func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayFlatMap(array,aBlk,fBlk,i), effects.star)(ctx)
  }
}
