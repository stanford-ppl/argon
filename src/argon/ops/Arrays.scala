package argon.ops

import argon.core.Staging
import org.virtualized.EmptyContext

trait ArrayApi extends ArrayExp with FixPtApi with VoidApi with TextApi {
  type Array[T <: MetaAny[T]] = MetaArray[T]

  // TODO: Change this - different than Scala syntax
  // Same as Array.empty[T](size)
  // def Array[T:Meta](size: Int32)(implicit ctx: SrcCtx): MetaArray[T] = MetaArray(array_new[T](size.s))
}

trait ArrayExp extends Staging with FixPtExp with VoidExp with TextExp with BoolExp {
  /** Infix methods **/
  case class MetaArray[T:Meta](s: Exp[MetaArray[T]]) extends MetaAny[MetaArray[T]] {
    def apply(i: Int32)(implicit ctx: SrcCtx): T = wrap(array_apply(s, i.s))
    def length(implicit ctx: SrcCtx): Int32 = wrap(array_length(s))

    def ===(that: MetaArray[T])(implicit ctx: SrcCtx): Bool = {
      val eqs = array_infix_zip(this,that,{(x: T,y: T) => x === y})
      array_infix_reduce(eqs, {(a,b) => a && b})
    }
    def =!=(that: MetaArray[T])(implicit ctx: SrcCtx): Bool = {
      val neqs = array_infix_zip(this,that, {(x: T, y: T) => x =!= y})
      array_infix_reduce(neqs, {(a,b) => a || b})
    }
    def toText(implicit ctx: SrcCtx) = textify(this)
  }

  private[argon] def array_infix_update[T:Meta](array: MetaArray[T], i: Index, data: T)(implicit ctx: SrcCtx): Void = {
    Void(array_update(array.s, i.s, data.s))
  }
  private[argon] def array_from_function[T:Meta](size: Index, func: Index => T)(implicit ctx: SrcCtx): MetaArray[T] = {
    val i = fresh[Index]
    val fBlk = Fun0(func(wrap(i)).s)
    MetaArray( array_mapindices(size.s, fBlk(), i) )
  }
  private[argon] def array_infix_foreach[T:Meta](array: MetaArray[T], func: T => Void)(implicit ctx: SrcCtx): Void = {
    val i = fresh[Index]
    val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result){ func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    Void( stageEffectful(ArrayForeach(array.s, aBlk, fBlk, i), effects.star)(ctx) )
  }
  private[argon] def array_infix_map[T:Meta,R:Meta](array: MetaArray[T], func: T => R)(implicit ctx: SrcCtx): MetaArray[R] = {
    val i = fresh[Index]
    val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result) { func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayMap(array.s, aBlk, fBlk, i), effects.star)(ctx)
    MetaArray(out)
  }
  private[argon] def array_infix_zip[T:Meta,S:Meta,R:Meta](a: MetaArray[T], b: MetaArray[S], func: (T,S) => R)(implicit ctx: SrcCtx): MetaArray[R] = {
    val i = fresh[Index]
    val aBlk = stageLambda(a.s) { a.apply(wrap(i)).s }
    val bBlk = stageLambda(b.s) { b.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func(wrap(aBlk.result),wrap(bBlk.result)).s }
    val effects = aBlk.summary andAlso bBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayZip(a.s,b.s,aBlk,bBlk,fBlk,i), effects.star)(ctx)
    MetaArray(out)
  }
  private[argon] def array_infix_reduce[T:Meta](array: MetaArray[T], reduce: (T,T) => T)(implicit ctx: SrcCtx): T = {
    val i = fresh[Index]
    val rV = (fresh[T],fresh[T])
    val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s }
    val rBlk = stageBlock { reduce(wrap(rV._1),wrap(rV._2)).s }
    val effects = aBlk.summary andAlso rBlk.summary
    val out = stageEffectful(ArrayReduce(array.s,aBlk,rBlk,i,rV),effects.star)(ctx)
    wrap(out)
  }
  private[argon] def array_infix_filter[T:Meta](array: MetaArray[T], filter: T => Bool)(implicit ctx: SrcCtx): MetaArray[T] = {
    val i = fresh[Index]
    val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s }
    val cBlk = stageLambda(aBlk.result) { filter(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso cBlk.summary
    val out = stageEffectful(ArrayFilter(array.s,aBlk,cBlk,i), effects.star)(ctx)
    MetaArray(out)
  }
  private[argon] def array_infix_flatMap[T:Meta,R:Meta](array: MetaArray[T], func: T => MetaArray[R])(implicit ctx: SrcCtx): MetaArray[R] = {
    val i = fresh[Index]
    val aBlk = stageLambda(array.s) { array.apply(wrap(i)).s }
    val fBlk = stageLambda(aBlk.result){ func(wrap(aBlk.result)).s }
    val effects = aBlk.summary andAlso fBlk.summary
    val out = stageEffectful(ArrayFlatMap(array.s,aBlk,fBlk,i), effects.star)(ctx)
    MetaArray(out)
  }

  private[argon] def input_arguments()(ctx: SrcCtx): MetaArray[Text] = wrap(stage(InputArguments())(ctx))

  /** Type classes **/
  // --- Staged
  case class ArrayType[T](child: Meta[T]) extends Meta[MetaArray[T]] {
    override def wrapped(s: Exp[MetaArray[T]]): MetaArray[T] = MetaArray(s)(child)
    override def typeArguments = List(child)
    override def stagedClass = classOf[MetaArray[T]]
    override def isPrimitive = false
  }
  implicit def arrayType[T:Meta]: Type[MetaArray[T]] = ArrayType(meta[T])

  /** IR Nodes **/
  case class InputArguments() extends Op[MetaArray[Text]] {
    def mirror(f:Tx) = stage(InputArguments())(EmptyContext)
  }

  case class ArrayNew[T:Type](size: Exp[Int32]) extends Op2[T,MetaArray[T]] {
    def mirror(f:Tx) = array_new[T](f(size))
  }

  case class ArrayApply[T:Type](array: Exp[MetaArray[T]], i: Exp[Int32]) extends Op[T] {
    def mirror(f:Tx) = array_apply(f(array),f(i))
    override def aliases = Nil
    //override def extracts = dyns(array) TODO: Why does this cause issues?
  }

  case class ArrayUpdate[T:Type](array: Exp[MetaArray[T]], i: Exp[Int32], e: Exp[T]) extends Op[Void] {
    def mirror(f:Tx) = array_update(f(array),f(i),f(e))
    override def contains = dyns(e)
  }

  case class ArrayLength[T:Type](array: Exp[MetaArray[T]]) extends Op[Int32] {
    def mirror(f:Tx) = array_length(f(array))
  }

  case class MapIndices[T:Type](size: Exp[Index], func: Block[T], i: Bound[Index]) extends Op[MetaArray[T]] {
    def mirror(f:Tx) = array_mapindices(f(size),f(func),i)
    override def inputs = dyns(size) ++ dyns(func)
    override def freqs  = normal(size) ++ hot(func)
    override def binds = i +: super.binds

    override def aliases = Nil

    val mT = typ[T]
  }

  case class ArrayForeach[T:Type](
    array: Exp[MetaArray[T]],
    apply: Block[T],
    func:  Block[Void],
    i:     Bound[Index]
  ) extends Op[Void] {
    def mirror(f:Tx) = array_foreach(f(array),f(apply),f(func),i)
    override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = i +: super.binds
    val mT = typ[T]
  }

  case class ArrayMap[T:Type,S:Type](
    array: Exp[MetaArray[T]],
    apply: Block[T],
    func:  Block[S],
    i: Bound[Index]
  ) extends Op[MetaArray[S]] {
    def mirror(f:Tx) = array_map(f(array),f(apply),f(func),i)
    override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = i +: super.binds

    override def aliases = Nil

    val mT = typ[T]
    val mS = typ[S]
  }

  case class ArrayZip[A:Type,B:Type,C:Type](
    arrayA: Exp[MetaArray[A]],
    arrayB: Exp[MetaArray[B]],
    applyA: Block[A],
    applyB: Block[B],
    func:   Block[C],
    i:      Bound[Index]
  ) extends Op[MetaArray[C]] {
    def mirror(f:Tx) = array_zip(f(arrayA),f(arrayB),f(applyA),f(applyB),f(func),i)
    override def inputs = dyns(arrayA) ++ dyns(arrayB) ++ dyns(applyA) ++ dyns(applyB) ++ dyns(func)
    override def freqs  = normal(arrayA) ++ normal(arrayB) ++ hot(applyA) ++ hot(applyB) ++ hot(func)
    override def binds = i +: super.binds

    override def aliases = Nil

    val mA = typ[A]
    val mB = typ[B]
    val mC = typ[C]
  }

  case class ArrayReduce[A:Type](
    array:  Exp[MetaArray[A]],
    apply:  Block[A],
    reduce: Block[A],
    i:      Bound[Index],
    rV:     (Bound[A],Bound[A])
  ) extends Op[A] {
    def mirror(f:Tx) = array_reduce(f(array),f(apply),f(reduce),i,rV)
    override def inputs = dyns(array) ++ dyns(apply) ++ dyns(reduce)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(reduce)
    override def binds = super.binds ++ Seq(i, rV._1, rV._2)

    val mA = typ[A]
  }

  case class ArrayFilter[A:Type](
    array: Exp[MetaArray[A]],
    apply: Block[A],
    cond:  Block[Bool],
    i:     Bound[Index]
  ) extends Op[MetaArray[A]] {
    def mirror(f:Tx) = array_filter(f(array),f(apply),f(cond),i)
    override def inputs = dyns(array) ++ dyns(apply) ++ dyns(cond)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(cond)
    override def binds = i +: super.binds

    override def aliases = Nil
  }

  case class ArrayFlatMap[A:Type,B:Type](
    array: Exp[MetaArray[A]],
    apply: Block[A],
    func:  Block[MetaArray[B]],
    i:     Bound[Index]
  ) extends Op[MetaArray[B]] {
    def mirror(f:Tx) = array_flatmap(f(array),f(apply),f(func),i)
    override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
    override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
    override def binds = i +: super.binds

    override def aliases = Nil
  }


  /** Constructors **/
  def array_new[T:Type](size: Exp[Int32])(implicit ctx: SrcCtx): Sym[MetaArray[T]] = {
    stageMutable(ArrayNew[T](size))(ctx)
  }
  def array_apply[T:Type](array: Exp[MetaArray[T]], i: Exp[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    stage(ArrayApply(array,i))(ctx)
  }
  def array_update[T:Type](array: Exp[MetaArray[T]], i: Exp[Int32], e: Exp[T])(implicit ctx: SrcCtx): Sym[Void] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }
  def array_length[T:Type](array: Exp[MetaArray[T]])(implicit ctx: SrcCtx): Sym[Int32] = {
    stage(ArrayLength(array))(ctx)
  }

  def array_mapindices[T:Type](
    size: Exp[Index],
    func: => Exp[T],
    i: Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MetaArray[T]] = {
    val blk = stageBlock{ func }
    val effects = blk.summary
    stageEffectful(MapIndices(size, blk, i), effects.star)(ctx)
  }

  def array_foreach[T:Type](
    array: Exp[MetaArray[T]],
    apply: => Exp[T],
    func: => Exp[Void],
    i: Bound[Index]
  )(implicit ctx: SrcCtx): Sym[Void] = {
    val aBlk = stageLambda(array){ apply }
    val fBlk = stageLambda(aBlk.result){ func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayForeach(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  def array_map[T:Type,R:Type](
    array: Exp[MetaArray[T]],
    apply: => Exp[T],
    func: => Exp[R],
    i: Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MetaArray[R]] = {
    val aBlk = stageLambda(array) { apply }
    val fBlk = stageLambda(aBlk.result) { func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayMap(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  def array_zip[A:Type,B:Type,C:Type](
    a: Exp[MetaArray[A]],
    b: Exp[MetaArray[B]],
    applyA: => Exp[A],
    applyB: => Exp[B],
    func:   => Exp[C],
    i:      Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MetaArray[C]] = {
    val aBlk = stageLambda(a) { applyA }
    val bBlk = stageLambda(b) { applyB }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func }
    val effects = aBlk.summary andAlso bBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayZip(a, b, aBlk, bBlk, fBlk, i), effects.star)(ctx)
  }

  def array_reduce[A:Type](
    array:  Exp[MetaArray[A]],
    apply:  => Exp[A],
    reduce: => Exp[A],
    i:      Bound[Index],
    rV:     (Bound[A],Bound[A])
  )(implicit ctx: SrcCtx): Sym[A] = {
    val aBlk = stageLambda(array) { apply }
    val rBlk = stageLambda(aBlk.result) { reduce }
    val effects = aBlk.summary andAlso rBlk.summary
    stageEffectful(ArrayReduce(array,aBlk,rBlk,i,rV), effects.star)(ctx)
  }

  def array_filter[A:Type](
    array: Exp[MetaArray[A]],
    apply: => Exp[A],
    cond:  => Exp[Bool],
    i:     Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MetaArray[A]] = {
    val aBlk = stageLambda(array) { apply }
    val cBlk = stageLambda(aBlk.result) { cond }
    val effects = aBlk.summary andAlso cBlk.summary
    stageEffectful(ArrayFilter(array,aBlk,cBlk,i), effects.star)(ctx)
  }

  def array_flatmap[A:Type,B:Type](
    array: Exp[MetaArray[A]],
    apply: => Exp[A],
    func:  => Exp[MetaArray[B]],
    i:     Bound[Index]
  )(implicit ctx: SrcCtx): Sym[MetaArray[B]] = {
    val aBlk = stageLambda(array) { apply }
    val fBlk = stageLambda(aBlk.result) { func }
    val effects = aBlk.summary andAlso fBlk.summary
    stageEffectful(ArrayFlatMap(array,aBlk,fBlk,i), effects.star)(ctx)
  }

  /** Internals **/
  override def recurseAtomicLookup(s: Exp[_]): Exp[_] = s match {
    case Def(ArrayApply(array,i)) => recurseAtomicLookup(array)
    case _ => super.recurseAtomicLookup(s)
  }
}
