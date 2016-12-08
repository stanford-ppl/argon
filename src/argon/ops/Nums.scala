package argon.ops

/** Staged numeric types **/
trait Nums extends Bits {
  type Num[T] <: Bit[T]
}
trait NumAPI extends Nums with BitAPI


trait NumExp extends Nums with BitExp {
  abstract class Num[T] extends Bit[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  def num[T:Num] = implicitly[Num[T]]
}

