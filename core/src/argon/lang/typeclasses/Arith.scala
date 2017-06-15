package argon.lang.typeclasses

import argon.internals._
import forge._

trait Arith[T] {
  @api def negate(x: T): T
  @api def plus(x: T, y: T): T
  @api def minus(x: T, y: T): T
  @api def times(x: T, y: T): T
  @api def divide(x: T, y: T): T
}

trait ArithExp {
  def arith[T:Arith] = implicitly[Arith[T]]
}
