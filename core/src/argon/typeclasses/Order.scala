package argon.typeclasses

import argon.core.Staging
import argon.ops._

/** Staged numeric types **/
trait OrderApi extends OrderExp { this: BoolApi =>
}

trait OrderExp extends Staging { this: BoolExp =>

  trait Order[T] {
    def lessThan(a: T, b: T)(implicit ctx: SrcCtx): Bool
    def lessThanOrEqual(a: T, b: T)(implicit ctx: SrcCtx): Bool
    def equal(a: T, b: T)(implicit ctx: SrcCtx): Bool
  }

  implicit class OrderInfixOps[T: Order](lhs: T) {
    def >(rhs: T)(implicit ctx: SrcCtx)         = ord[T].lessThan(rhs, lhs)
    def >=(rhs: T)(implicit ctx: SrcCtx)        = ord[T].lessThanOrEqual(rhs, lhs)
    def <(rhs: T)(implicit ctx: SrcCtx)         = ord[T].lessThan(lhs, rhs)
    def <=(rhs: T)(implicit ctx: SrcCtx)        = ord[T].lessThanOrEqual(lhs, rhs)
    def ===(rhs: T)(implicit ctx: SrcCtx): Bool = ord[T].equal(lhs, rhs)
    def =!=(rhs: T)(implicit ctx: SrcCtx): Bool = !(lhs === rhs)
  }

  def ord[T: Order] = implicitly[Order[T]]
}
