package argon.lang.typeclasses

import forge._

trait Arith[T] {
  /** Returns the negation of the given value. **/
  @api def negate(x: T): T

  /** Returns the result of adding `x` and `y`. **/
  @api def plus(x: T, y: T): T

  /** Returns the result of subtracting `y` from `x`. **/
  @api def minus(x: T, y: T): T

  /** Returns the result of multiplying `x` and `y`. **/
  @api def times(x: T, y: T): T

  /** Returns the result of dividing `x` by `y`. **/
  @api def divide(x: T, y: T): T
}
