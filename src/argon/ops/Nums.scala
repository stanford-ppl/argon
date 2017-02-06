package argon.ops

/** Staged numeric types **/
trait NumOps extends OrderOps { this: TextOps =>
  type Num[T] <: Order[T]
}
trait NumApi extends NumOps with OrderApi {this: TextApi => }


trait NumExp extends NumOps with OrderExp { this: TextExp =>
  trait Num[T] extends Order[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
    def divide(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  implicit class Ops[T:Num](lhs: T) {
    def unary_-(implicit ctx: SrcCtx): T = num[T].negate(lhs)
    def +(rhs: T)(implicit ctx: SrcCtx): T = num[T].plus(lhs, rhs)
    def -(rhs: T)(implicit ctx: SrcCtx): T = num[T].minus(lhs, rhs)
    def *(rhs: T)(implicit ctx: SrcCtx): T = num[T].times(lhs, rhs)
    def /(rhs: T)(implicit ctx: SrcCtx): T = num[T].divide(lhs, rhs)
  }

  def num[T:Num] = implicitly[Num[T]]
}

