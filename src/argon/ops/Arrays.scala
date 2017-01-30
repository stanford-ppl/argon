package argon.ops

trait ArrayOps extends FixPtOps with VoidOps with TextOps {
  type StagedArray[T] <: StagedArrayOps[T]
  type MArray[T] = StagedArray[T]

  protected trait StagedArrayOps[T] {
    def apply(i: Int32)(implicit ctx: SrcCtx): T
    def length(implicit ctx: SrcCtx): Int32
  }

  def createArray[T:Staged](size: Int32)(implicit ctx: SrcCtx): MArray[T]

  implicit def arrayType[T:Staged]: Staged[StagedArray[T]]
}

trait ArrayApi extends ArrayOps with FixPtApi with VoidApi with TextApi {
  type Array[T] = StagedArray[T]

  def Array[T:Staged](size: Int32)(implicit ctx: SrcCtx): MArray[T] = createArray[T](size)
}

trait ArrayExp extends ArrayOps with FixPtExp with VoidExp with TextExp {
  /** API **/
  case class StagedArray[T:Staged](s: Exp[StagedArray[T]]) extends StagedArrayOps[T] {
    def apply(i: Int32)(implicit ctx: SrcCtx): T = wrap(array_apply(s, i.s))
    def length(implicit ctx: SrcCtx): Int32 = wrap(array_length(s))
  }

  def createArray[T:Staged](size: Int32)(implicit ctx: SrcCtx): MArray[T] = StagedArray(array_new[T](size.s))

  /** Staged Types **/
  case class ArrayType[T](child: Staged[T]) extends Staged[StagedArray[T]] {
    override def wrapped(s: Exp[StagedArray[T]]): StagedArray[T] = StagedArray(s)(child)
    override def unwrapped(x: StagedArray[T]) = x.s
    override def typeArguments = List(child)
    override def stagedClass = classOf[StagedArray[T]]
    override def isPrimitive = false
  }
  implicit def arrayType[T:Staged]: Staged[StagedArray[T]] = ArrayType(typ[T])

  /** IR Nodes **/
  case class InputArguments() extends Op[MArray[Text]] { def mirror(f:Tx) = stage(InputArguments())(here) }

  case class ArrayNew[T:Staged](size: Exp[Int32]) extends Op2[T,MArray[T]] {
    def mirror(f:Tx) = array_new[T](f(size))
  }

  case class ArrayApply[T:Staged](array: Exp[MArray[T]], i: Exp[Int32]) extends Op[T] {
    def mirror(f:Tx) = array_apply(f(array),f(i))
  }
  case class ArrayUpdate[T:Staged](array: Exp[MArray[T]], i: Exp[Int32], e: Exp[T]) extends Op[Void] {
    def mirror(f:Tx) = array_update(f(array),f(i),f(e))
  }
  case class ArrayLength[T:Staged](array: Exp[MArray[T]]) extends Op[Int32] {
    def mirror(f:Tx) = array_length(f(array))
  }

  /** Smart Constructors **/
  def array_new[T:Staged](size: Exp[Int32])(implicit ctx: SrcCtx): Sym[MArray[T]] = {
    stageMutable(ArrayNew[T](size))(ctx)
  }
  def array_apply[T:Staged](array: Exp[MArray[T]], i: Exp[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    stage(ArrayApply(array,i))(ctx)
  }
  def array_update[T:Staged](array: Exp[MArray[T]], i: Exp[Int32], e: Exp[T])(implicit ctx: SrcCtx): Sym[Void] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }
  def array_length[T:Staged](array: Exp[MArray[T]])(implicit ctx: SrcCtx): Sym[Int32] = {
    stage(ArrayLength(array))(ctx)
  }
}
