package argon.typeclasses

import forge._

trait Arith[T] {
  @api def negate(x: T): T
  @api def plus(x: T, y: T): T
  @api def minus(x: T, y: T): T
  @api def times(x: T, y: T): T
  @api def divide(x: T, y: T): T
}

trait ArithExp {
  implicit class ArithOps[T:Arith](lhs: T) {
    @api def unary_-(): T = arith[T].negate(lhs)
    @api def +(rhs: T): T = arith[T].plus(lhs, rhs)
    @api def -(rhs: T): T = arith[T].minus(lhs, rhs)
    @api def *(rhs: T): T = arith[T].times(lhs, rhs)
    @api def /(rhs: T): T = arith[T].divide(lhs, rhs)
  }

  def arith[T:Arith] = implicitly[Arith[T]]
}

