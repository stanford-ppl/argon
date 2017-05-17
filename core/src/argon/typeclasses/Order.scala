package argon.typeclasses

import argon._
import argon.nodes._
import forge._

/** Staged types with ordering **/
trait Order[T] {
  @api def lessThan(a: T, b: T): MBoolean
  @api def lessThanOrEqual(a: T, b: T): MBoolean
  @api def equal(a: T, b: T): MBoolean
}

trait OrderExp {

  implicit class OrderInfixOps[T:Order](lhs: T) {
    @api def > (rhs: T): MBoolean = ord[T].lessThan(rhs, lhs)
    @api def >=(rhs: T): MBoolean = ord[T].lessThanOrEqual(rhs, lhs)
    @api def < (rhs: T): MBoolean = ord[T].lessThan(lhs, rhs)
    @api def <=(rhs: T): MBoolean = ord[T].lessThanOrEqual(lhs, rhs)
    @api def ===(rhs: T): MBoolean = ord[T].equal(lhs, rhs)
    @api def =!=(rhs: T): MBoolean = !(lhs === rhs)
  }

  def ord[T:Order] = implicitly[Order[T]]
}


