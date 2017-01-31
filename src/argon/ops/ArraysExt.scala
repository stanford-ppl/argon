package argon.ops

/**
  * Array operations are separated into two categories. ArrayOps is for use in all DSLs which allow at least the use
  * of command-line arguments. ArrayExtOps is for more complicated operations, including update and parallel patterns.
  */
trait ArrayExtOps extends ArrayOps {
  implicit class ArrayInfixOps[T:Staged](a: MArray[T]) {
    def update[A](i: Index, data: A)(implicit ctx: SrcCtx, lft: Lift[A,T]): Void = array_infix_update(a, i, lft.lift(data))
    def foreach(func: T => Void)(implicit ctx: SrcCtx): Void = array_infix_foreach(a, func)
    def map[R:Staged](func: T => R)(implicit ctx: SrcCtx): MArray[R] = array_infix_map(a, func)
    def zip[S:Staged,R:Staged](b: MArray[S])(func: (T,S) => R)(implicit ctx: SrcCtx): MArray[R] = array_infix_zip(a, b, func)
    def reduce(rfunc: (T,T) => T)(implicit ctx: SrcCtx): T = array_infix_reduce(a, rfunc)
    def filter(cond: T => Bool)(implicit ctx: SrcCtx): MArray[T] = array_infix_filter(a, cond)
    def flatMap[R:Staged](func: T => MArray[R])(implicit ctx: SrcCtx): MArray[R] = array_infix_flatMap(a, func)
  }

  private[argon] def array_infix_update[T:Staged](array: MArray[T], i: Index, data: T)(implicit ctx: SrcCtx): Void
  private[argon] def array_from_function[T:Staged](size: Index, func: Index => T)(implicit ctx: SrcCtx): MArray[T]
  private[argon] def array_infix_foreach[T:Staged](array: MArray[T], func: T => Void)(implicit ctx: SrcCtx): Void
  private[argon] def array_infix_map[T:Staged,R:Staged](array: MArray[T], func: T => R)(implicit ctx: SrcCtx): MArray[R]
  private[argon] def array_infix_zip[T:Staged,S:Staged,R:Staged](a: MArray[T], b: MArray[S], func: (T,S) => R)(implicit ctx: SrcCtx): MArray[R]
  private[argon] def array_infix_reduce[T:Staged](array: MArray[T], reduce: (T,T) => T)(implicit ctx: SrcCtx): T
  private[argon] def array_infix_filter[T:Staged](array: MArray[T], filter: T => Bool)(implicit ctx: SrcCtx): MArray[T]
  private[argon] def array_infix_flatMap[T:Staged,R:Staged](array: MArray[T], func: T => MArray[R])(implicit ctx: SrcCtx): MArray[R]
}

trait ArrayExtApi extends ArrayExtOps with ArrayApi {
  object Array {
    def tabulate[T:Staged](size: Index)(func: Index => T)(implicit ctx: SrcCtx): MArray[T] = array_from_function[T](size, func)
    def fill[T:Staged](size: Index)(func: => T)(implicit ctx: SrcCtx): MArray[T] = Array.tabulate(size){i => func}
  }
}

trait ArrayExtExp extends ArrayExtOps with ArrayExp {

  /** API **/
  private[argon] def array_infix_update[T:Staged](array: MArray[T], i: Index, data: T)(implicit ctx: SrcCtx): Void = {
    Void(array_update(array.s, i.s, data.s))
  }
  private[argon] def array_from_function[T:Staged](size: Index, func: Index => T)(implicit ctx: SrcCtx): MArray[T] = {
    val i = fresh[Index]
    val fBlk = () => func(wrap(i)).s
    StagedArray( array_mapindices(size.s, fBlk(), i) )
  }
  private[argon] def array_infix_foreach[T:Staged](array: MArray[T], func: T => Void)(implicit ctx: SrcCtx): Void = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result){ func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    Void( stageEffectful(ArrayForeach(array.s, aBlk, fBlk, i), effects.star)(ctx) )
  }
  private[argon] def array_infix_map[T:Staged,R:Staged](array: MArray[T], func: T => R)(implicit ctx: SrcCtx): MArray[R] = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result) { func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayMap(array.s, aBlk, fBlk, i), effects.star)(ctx)
    StagedArray(out)
  }
  private[argon] def array_infix_zip[T:Staged,S:Staged,R:Staged](a: MArray[T], b: MArray[S], func: (T,S) => R)(implicit ctx: SrcCtx): MArray[R] = {
    val i = fresh[Index]
    val aBlk = stageBlock { a.apply(wrap(i)).s }
    val bBlk = stageBlock { b.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func(wrap(aBlk.result),wrap(bBlk.result)).s }
    val effects = aBlk.summary andAlso bBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayZip(a.s,b.s,aBlk,bBlk,fBlk,i), effects.star)(ctx)
    StagedArray(out)
  }
  private[argon] def array_infix_reduce[T:Staged](array: MArray[T], reduce: (T,T) => T)(implicit ctx: SrcCtx): T = {
    val i = fresh[Index]
    val rV = (fresh[T],fresh[T])
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val rBlk = stageBlock { reduce(wrap(rV._1),wrap(rV._2)).s }
    val effects = aBlk.summary andAlso rBlk.summary
    val out = stageEffectful(ArrayReduce(array.s,aBlk,rBlk,i,rV),effects.star)(ctx)
    wrap(out)
  }
  private[argon] def array_infix_filter[T:Staged](array: MArray[T], filter: T => Bool)(implicit ctx: SrcCtx): MArray[T] = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val cBlk = stageLambda(aBlk.result) { filter(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso cBlk.summary
    val out = stageEffectful(ArrayFilter(array.s,aBlk,cBlk,i), effects.star)(ctx)
    StagedArray(out)
  }
  private[argon] def array_infix_flatMap[T:Staged,R:Staged](array: MArray[T], func: T => MArray[R])(implicit ctx: SrcCtx): MArray[R] = {
    val i = fresh[Index]
    val aBlk = stageBlock { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result){ func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayFlatMap(array.s,aBlk,fBlk,i), effects.star)(ctx)
    StagedArray(out)
  }

  /** IR Nodes **/
  case class ArrayUpdate[T:Staged](array: Exp[MArray[T]], i: Exp[Int32], e: Exp[T]) extends Op[Void] {
    def mirror(f:Tx) = array_update(f(array),f(i),f(e))
  }
  case class MapIndices[T:Staged](size: Exp[Index], func: Block[T], i: Bound[Index]) extends Op[MArray[T]] {
    def mirror(f:Tx) = array_mapindices(f(size),f(func),i)
    override def inputs = syms(size) ++ syms(func)
    override def freqs  = normal(size) ++ hot(func)
    override def binds = super.binds :+ i
    val mT = typ[T]
  }
  case class ArrayForeach[T:Staged](
    array: Exp[MArray[T]],
    apply: Block[T],
    func:  Block[Void],
    i:     Bound[Index]
  ) extends Op[Void] {
    def mirror(f:Tx) = array_foreach(f(array),f(apply),f(func),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = super.binds :+ i
    override def tunnels = syms(array)
    val mT = typ[T]
  }
  case class ArrayMap[T:Staged,S:Staged](
    array: Exp[MArray[T]],
    apply: Block[T],
    func:  Block[S],
    i: Bound[Index]
  ) extends Op[MArray[S]] {
    def mirror(f:Tx) = array_map(f(array),f(apply),f(func),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = super.binds :+ i
    override def tunnels = syms(array)
    val mT = typ[T]
    val mS = typ[S]
  }

  case class ArrayZip[A:Staged,B:Staged,C:Staged](
    arrayA: Exp[MArray[A]],
    arrayB: Exp[MArray[B]],
    applyA: Block[A],
    applyB: Block[B],
    func:   Block[C],
    i:      Bound[Index]
  ) extends Op[MArray[C]] {
    def mirror(f:Tx) = array_zip(f(arrayA),f(arrayB),f(applyA),f(applyB),f(func),i)
    override def inputs = syms(arrayA) ++ syms(arrayB) ++ syms(applyA) ++ syms(applyB) ++ syms(func)
    override def freqs  = normal(arrayA) ++ normal(arrayB) ++ hot(applyA) ++ hot(applyB) ++ hot(func)
    override def binds = super.binds :+ i
    override def tunnels = syms(arrayA) ++ syms(arrayB)
    val mA = typ[A]
    val mB = typ[B]
    val mC = typ[C]
  }

  case class ArrayReduce[A:Staged](
    array:  Exp[MArray[A]],
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
    val mA = typ[A]
  }

  case class ArrayFilter[A:Staged](
    array: Exp[MArray[A]],
    apply: Block[A],
    cond:  Block[Bool],
    i:     Bound[Index]
  ) extends Op[MArray[A]] {
    def mirror(f:Tx) = array_filter(f(array),f(apply),f(cond),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(cond)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(cond)
    override def binds = super.binds :+ i
    override def tunnels = syms(array)
  }

  case class ArrayFlatMap[A:Staged,B:Staged](
    array: Exp[MArray[A]],
    apply: Block[A],
    func:  Block[MArray[B]],
    i:     Bound[Index]
  ) extends Op[MArray[B]] {
    def mirror(f:Tx) = array_flatmap(f(array),f(apply),f(func),i)
    override def inputs = syms(array) ++ syms(apply) ++ syms(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = super.binds :+ i
    override def tunnels = syms(array)
  }


  /** Constructors **/
  def array_update[T:Staged](array: Exp[MArray[T]], i: Exp[Int32], e: Exp[T])(implicit ctx: SrcCtx): Sym[Void] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }

  def array_mapindices[T:Staged](size: Exp[Index], func: => Exp[T], i: Bound[Index])(implicit ctx: SrcCtx): Sym[MArray[T]] = {
    val blk = stageBlock{ func }
    val effects = blk.summary
    stageEffectful(MapIndices(size, blk, i), effects.star)(ctx)
  }

  def array_foreach[T:Staged](array: Exp[MArray[T]], apply: => Exp[T], func: => Exp[Void], i: Bound[Index])(implicit ctx: SrcCtx): Sym[Void] = {
    val aBlk = stageBlock{ apply }
    val fBlk = stageLambda(aBlk.result){ func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayForeach(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  def array_map[T:Staged,R:Staged](array: Exp[MArray[T]], apply: => Exp[T], func: => Exp[R], i: Bound[Index])(implicit ctx: SrcCtx): Sym[MArray[R]] = {
    val aBlk = stageBlock { apply }
    val fBlk = stageLambda(aBlk.result) { func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayMap(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  def array_zip[A:Staged,B:Staged,C:Staged](
    a: Exp[MArray[A]],
    b: Exp[MArray[B]],
    applyA: => Exp[A],
    applyB: => Exp[B],
    func:   => Exp[C],
    i:      Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MArray[C]] = {
    val aBlk = stageBlock { applyA }
    val bBlk = stageBlock { applyB }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func }
    val effects = aBlk.summary andAlso bBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayZip(a, b, aBlk, bBlk, fBlk, i), effects.star)(ctx)
  }

  def array_reduce[A:Staged](
    array:  Exp[MArray[A]],
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

  def array_filter[A:Staged](
    array: Exp[MArray[A]],
    apply: => Exp[A],
    cond:  => Exp[Bool],
    i:     Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MArray[A]] = {
    val aBlk = stageBlock { apply }
    val cBlk = stageLambda(aBlk.result) { cond }
    val effects = aBlk.summary andAlso cBlk.summary
    stageEffectful(ArrayFilter(array,aBlk,cBlk,i), effects.star)(ctx)
  }

  def array_flatmap[A:Staged,B:Staged](
    array: Exp[MArray[A]],
    apply: => Exp[A],
    func:  => Exp[MArray[B]],
    i:     Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MArray[B]] = {
    val aBlk = stageBlock { apply }
    val fBlk = stageLambda(aBlk.result) { func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayFlatMap(array,aBlk,fBlk,i), effects.star)(ctx)
  }

}
