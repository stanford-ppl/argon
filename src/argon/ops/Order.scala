package argon.ops

/** Staged numeric types **/
trait OrderOps extends BitsOps with BoolOps { this: TextOps =>
  type Order[T] <: Staged[T]
}
trait OrderApi extends OrderOps with BitsApi with BoolApi { this: TextApi => }


trait OrderExp extends OrderOps with BitsExp with BoolExp { this: TextExp =>
  trait Order[T] extends Staged[T] {
    def lessThan(a: T, b: T)(implicit ctx: SrcCtx): Bool
    def lessEql(a: T, b: T)(implicit ctx: SrcCtx): Bool
    def equal(a: T, b: T)(implicit ctx: SrcCtx): Bool
  }

  implicit class OrderInfixOps[T:Order](lhs: T) {
    def > (rhs: T)(implicit ctx: SrcCtx) = ord[T].lessThan(rhs, lhs)
    def >=(rhs: T)(implicit ctx: SrcCtx) = ord[T].lessEql(rhs, lhs)
    def < (rhs: T)(implicit ctx: SrcCtx) = ord[T].lessThan(lhs, rhs)
    def <=(rhs: T)(implicit ctx: SrcCtx) = ord[T].lessEql(lhs, rhs)
  }
  def infix_==[T:Order](lhs: T, rhs: T)(implicit ctx: SrcCtx): Bool = ord[T].equal(lhs, rhs)
  def infix_!=[T:Order](lhs: T, rhs: T)(implicit ctx: SrcCtx): Bool = !ord[T].equal(lhs, rhs)

  def ord[T:Order] = implicitly[Order[T]]
}

