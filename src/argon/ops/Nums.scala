package argon.ops

/** Staged numeric types **/
trait NumOps extends OrderOps { this: TextOps =>
  type Num[T] <: Order[T]
}
trait NumApi extends NumOps with OrderApi {this: TextApi => }


trait NumExp extends NumOps with OrderExp { this: TextExp =>
  trait Num[T] extends Order[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  def num[T:Num] = implicitly[Num[T]]
}

