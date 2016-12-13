package argon.ops

/** Staged numeric types **/
trait Nums extends Orders {
  type Num[T] <: Order[T]
}
trait NumApi extends Nums with OrderApi


trait NumExp extends Nums with OrderExp {
  trait Num[T] extends Order[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  def num[T:Num] = implicitly[Num[T]]
}

