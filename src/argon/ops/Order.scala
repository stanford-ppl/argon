package argon.ops

/** Staged numeric types **/
trait Orders extends BitsOps with Bools {
  type Order[T] <: Bits[T]
}
trait OrderApi extends Orders with BitsApi with BoolApi


trait OrderExp extends Orders with BitsExp with BoolExp {
  trait Order[T] extends Bits[T] {
    def lessThan(a: T, b: T)(implicit ctx: SrcCtx): Bool
    def equal(a: T, b: T)(implicit ctx: SrcCtx): Bool
  }

  def ord[T:Order] = implicitly[Order[T]]
}

