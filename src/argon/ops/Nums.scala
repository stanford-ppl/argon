package argon.ops

/** Staged numeric types **/
trait NumOps extends BitsOps with OrderOps { this: TextOps =>
  type Arith[T] <: Bits[T]
  type Num[T] <: Arith[T] with Order[T]
}
trait NumApi extends NumOps with BitsApi with OrderApi {this: TextApi => }


trait NumExp extends NumOps with BitsExp with OrderExp { this: TextExp =>
  trait Arith[T] extends Bits[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
    def divide(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  implicit class Ops[T:Arith](lhs: T) {
    def unary_-(implicit ctx: SrcCtx): T = arith[T].negate(lhs)
    def +(rhs: T)(implicit ctx: SrcCtx): T = arith[T].plus(lhs, rhs)
    def -(rhs: T)(implicit ctx: SrcCtx): T = arith[T].minus(lhs, rhs)
    def *(rhs: T)(implicit ctx: SrcCtx): T = arith[T].times(lhs, rhs)
    def /(rhs: T)(implicit ctx: SrcCtx): T = arith[T].divide(lhs, rhs)
  }


  trait Num[T] extends Arith[T] with Order[T]

  def arith[T:Arith] = implicitly[Arith[T]]
  def num[T:Num] = implicitly[Num[T]]
}

