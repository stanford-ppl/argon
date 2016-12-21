package argon.ops

/** Staged numeric types **/
trait OrderOps extends BitsOps with BoolOps {
  type Order[T] <: Bits[T]
}
trait OrderApi extends OrderOps with BitsApi with BoolApi


trait OrderExp extends OrderOps with BitsExp with BoolExp {
  trait Order[T] extends Bits[T] {
    def lessThan(a: T, b: T)(implicit ctx: SrcCtx): Bool
    def equal(a: T, b: T)(implicit ctx: SrcCtx): Bool
  }

  def ord[T:Order] = implicitly[Order[T]]
}

