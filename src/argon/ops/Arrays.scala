package argon.ops

import argon.core.Staging

trait ArrayApi extends ArrayExp with FixPtApi with VoidApi with TextApi {
  type Array[T <: StageAny[T]] = ArgonArray[T]

  // Same as Array.empty[T](size)
  def Array[T <: StageAny[T] : FStaged](size: Int32)(implicit ctx: SrcCtx): ArgonArray[T] = ArgonArray(array_new[T](size.s))
}

trait ArrayExp extends Staging with FixPtExp with VoidExp with TextExp {
  /** Infix methods **/
  case class ArgonArray[T <: StageAny[T] : FStaged](s: Exp[ArgonArray[T]]) extends StageAny[ArgonArray[T]] {
    def apply(i: Int32)(implicit ctx: SrcCtx): T = wrap(array_apply(s, i.s))
    def length(implicit ctx: SrcCtx): Int32 = wrap(array_length(s))
  }

  /** Type classes **/
  // --- Staged
  case class ArrayType[T <: StageAny[T]](child: Staged[T]) extends FStaged[ArgonArray[T]] {
    override def wrapped(s: Exp[ArgonArray[T]]): ArgonArray[T] = ArgonArray(s)(child)
    override def typeArguments = List(child)
    override def stagedClass = classOf[ArgonArray[T]]
    override def isPrimitive = false
  }
  implicit def arrayType[T <: StageAny[T] : Staged]: FStaged[ArgonArray[T]] = ArrayType(ftyp[T])

  /** IR Nodes **/
  case class InputArguments() extends Op[ArgonArray[Text]] { def mirror(f:Tx) = stage(InputArguments())(here) }

  case class ArrayNew[T <: StageAny[T] : FStaged](size: Exp[Int32]) extends Op2[T, ArgonArray[T]] {
    def mirror(f:Tx) = array_new[T](f(size))
  }

  case class ArrayApply[T <: StageAny[T] : FStaged](array: Exp[ArgonArray[T]], i: Exp[Int32]) extends Op[T] {
    def mirror(f:Tx) = array_apply(f(array),f(i))
    override def aliases = Nil
    //override def extracts = syms(array) TODO: Why does this cause issues?
  }

  case class ArrayLength[T <: StageAny[T] : FStaged](array: Exp[ArgonArray[T]]) extends Op[Int32] {
    def mirror(f:Tx) = array_length(f(array))
  }

  /** Constructors **/
  def array_new[T <: StageAny[T] : FStaged](size: Exp[Int32])(implicit ctx: SrcCtx): Sym[ArgonArray[T]] = {
    stageMutable(ArrayNew[T](size))(ctx)
  }
  def array_apply[T <: StageAny[T] : FStaged](array: Exp[ArgonArray[T]], i: Exp[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    stage(ArrayApply(array,i))(ctx)
  }

  def array_length[T <: StageAny[T] : FStaged](array: Exp[ArgonArray[T]])(implicit ctx: SrcCtx): Sym[Int32] = {
    stage(ArrayLength(array))(ctx)
  }

  /** Internals **/
  override def recurseAtomicLookup(s: Exp[_]): Exp[_] = s match {
    case Def(ArrayApply(array,i)) => recurseAtomicLookup(array)
    case _ => super.recurseAtomicLookup(s)
  }
}
