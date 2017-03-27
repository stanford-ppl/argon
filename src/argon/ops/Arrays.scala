package argon.ops

import argon.core.Staging

trait ArrayApi extends ArrayExp with FixPtApi with VoidApi with TextApi {
  type Array[T] = ArgonArray[T]

  // Same as Array.empty[T](size)
  def Array[T:Type](size: Int32)(implicit ctx: SrcCtx): ArgonArray[T] = ArgonArray(array_new[T](size.s))
}

trait ArrayExp extends Staging with FixPtExp with VoidExp with TextExp {
  /** Infix methods **/
  case class ArgonArray[T:Type](s: Exp[ArgonArray[T]]) {
    def apply(i: Int32)(implicit ctx: SrcCtx): T = wrap(array_apply(s, i.s))
    def length(implicit ctx: SrcCtx): Int32 = wrap(array_length(s))
  }

  /** Type classes **/
  // --- Staged
  case class ArrayType[T](child: Type[T]) extends Type[ArgonArray[T]] {
    override def wrapped(s: Exp[ArgonArray[T]]): ArgonArray[T] = ArgonArray(s)(child)
    override def unwrapped(x: ArgonArray[T]) = x.s
    override def typeArguments = List(child)
    override def stagedClass = classOf[ArgonArray[T]]
    override def isPrimitive = false
  }
  implicit def arrayType[T:Type]: Type[ArgonArray[T]] = ArrayType(typ[T])

  /** IR Nodes **/
  case class InputArguments() extends Op[ArgonArray[Text]] { def mirror(f:Tx) = stage(InputArguments())(here) }

  case class ArrayNew[T:Type](size: Exp[Int32]) extends Op2[T,ArgonArray[T]] {
    def mirror(f:Tx) = array_new[T](f(size))
  }

  case class ArrayApply[T:Type](array: Exp[ArgonArray[T]], i: Exp[Int32]) extends Op[T] {
    def mirror(f:Tx) = array_apply(f(array),f(i))
    override def aliases = Nil
    //override def extracts = dyns(array) TODO: Why does this cause issues?
  }

  case class ArrayLength[T:Type](array: Exp[ArgonArray[T]]) extends Op[Int32] {
    def mirror(f:Tx) = array_length(f(array))
  }

  /** Constructors **/
  def array_new[T:Type](size: Exp[Int32])(implicit ctx: SrcCtx): Sym[ArgonArray[T]] = {
    stageMutable(ArrayNew[T](size))(ctx)
  }
  def array_apply[T:Type](array: Exp[ArgonArray[T]], i: Exp[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    stage(ArrayApply(array,i))(ctx)
  }

  def array_length[T:Type](array: Exp[ArgonArray[T]])(implicit ctx: SrcCtx): Sym[Int32] = {
    stage(ArrayLength(array))(ctx)
  }

  /** Internals **/
  override def recurseAtomicLookup(s: Exp[_]): Exp[_] = s match {
    case Def(ArrayApply(array,i)) => recurseAtomicLookup(array)
    case _ => super.recurseAtomicLookup(s)
  }
}
