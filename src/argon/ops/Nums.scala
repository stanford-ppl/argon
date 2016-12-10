package argon.ops

/** Staged numeric types **/
trait Nums extends BitsOps {
  type Num[T] <: Bits[T]
}
trait NumApi extends Nums with BitsApi


trait NumExp extends Nums with BitsExp {
  abstract class Num[T] extends Bits[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  def num[T:Num] = implicitly[Num[T]]
}

