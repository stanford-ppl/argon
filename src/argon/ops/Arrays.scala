package argon.ops

trait ArrayOps extends FixPtOps with VoidOps with TextOps {
  type StagedArray[T] <: StagedArrayOps[T]
  type MArray[T] = StagedArray[T]

  protected trait StagedArrayOps[T] {
    def apply(i: Int32)(implicit ctx: SrcCtx): T
    def update(i: Int32, e: T)(implicit ctx: SrcCtx): Void
  }

  def Array[T:Staged](size: Int32)(implicit ctx: SrcCtx): MArray[T]

  implicit def arrayType[T:Staged]: Staged[StagedArray[T]]
}

trait ArrayApi extends ArrayOps with FixPtApi with VoidApi with TextApi {
  type Array[T] = StagedArray[T]
}

trait ArrayExp extends ArrayOps with FixPtExp with VoidExp with TextExp {
  /** API **/
  case class StagedArray[T:Staged](s: Sym[StagedArray[T]]) extends StagedArrayOps[T] {
    def apply(i: Int32)(implicit ctx: SrcCtx): T = wrap(array_apply(s, i.s))
    def update(i: Int32, e: T)(implicit ctx: SrcCtx): Void = Void(array_update(s, i.s, e.s))
  }

  def Array[T:Staged](size: Int32)(implicit ctx: SrcCtx): MArray[T] = StagedArray(array_new[T](size.s))

  /** Staged Types **/
  class ArrayType[T:Staged] extends Staged[StagedArray[T]] {
    override def wrapped(s: Sym[StagedArray[T]]): StagedArray[T] = StagedArray(s)
    override def unwrapped(x: StagedArray[T]) = x.s
    override def typeArguments = List(typ[T])
    override def stagedClass = classOf[StagedArray[T]]
    override def isPrimitive = false
  }
  implicit def arrayType[T:Staged]: Staged[StagedArray[T]] = new ArrayType[T]

  /** IR Nodes **/
  case class InputArguments() extends Op[MArray[Text]] { def mirror(f:Tx) = stage(InputArguments())(here) }

  case class ArrayNew[T:Staged](size: Sym[Int32]) extends Op[MArray[T]] {
    def mirror(f:Tx) = array_new[T](f(size))
  }

  case class ArrayApply[T:Staged](array: Sym[MArray[T]], i: Sym[Int32]) extends Op[T] {
    def mirror(f:Tx) = array_apply(f(array),f(i))
  }
  case class ArrayUpdate[T:Staged](array: Sym[MArray[T]], i: Sym[Int32], e: Sym[T]) extends Op[Void] {
    def mirror(f:Tx) = array_update(f(array),f(i),f(e))
  }

  /** Smart Constructors **/
  def array_new[T:Staged](size: Sym[Int32])(implicit ctx: SrcCtx): Sym[MArray[T]] = {
    stageMutable(ArrayNew[T](size))(ctx)
  }
  def array_apply[T:Staged](array: Sym[MArray[T]], i: Sym[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    stage(ArrayApply(array,i))(ctx)
  }
  def array_update[T:Staged](array: Sym[MArray[T]], i: Sym[Int32], e: Sym[T])(implicit ctx: SrcCtx): Sym[Void] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }
}
