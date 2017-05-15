package argon.typeclasses

import argon._
import argon.lang.Bool
import forge._

/** Staged types with ordering **/
trait Order[T] {
  @api def lessThan(a: T, b: T): Bool
  @api def lessThanOrEqual(a: T, b: T): Bool
  @api def equal(a: T, b: T): Bool
}

trait OrderExp {

  implicit class OrderInfixOps[T:Order](lhs: T) {
    @api def > (rhs: T): Bool = ord[T].lessThan(rhs, lhs)
    @api def >=(rhs: T): Bool = ord[T].lessThanOrEqual(rhs, lhs)
    @api def < (rhs: T): Bool = ord[T].lessThan(lhs, rhs)
    @api def <=(rhs: T): Bool = ord[T].lessThanOrEqual(lhs, rhs)
    @api def ===(rhs: T): Bool = ord[T].equal(lhs, rhs)
    @api def =!=(rhs: T): Bool = !(lhs === rhs)
  }

  def ord[T:Order] = implicitly[Order[T]]
}


