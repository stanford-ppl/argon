package argon.ops

trait CastOps extends NumOps {
  implicit class CastOps[T:Num](x: T) {
    def to[R:Num](implicit ctx: SrcCtx): R = cast[T,R](x)
  }
  implicit class ConstCastOps[T:Numeric](x: T) {
    def as[R:Num](implicit ctx: SrcCtx): R = castLift[R](x)
  }

  protected def cast[T:Num,R:Num](x: T)(implicit ctx: SrcCtx): R
  protected def castLift[R:Num](x: Any)(implicit ctx: SrcCtx): R
}

trait CastApi extends CastOps with NumApi


trait CastExp extends CastOps with NumExp {

  protected def cast[T:Num,R:Num](x: T)(implicit ctx: SrcCtx): R = (num[T],num[R]) match {
    case (a,b) if a == b => x.asInstanceOf[R]
    case (a,b) => new UnsupportedCastError(a,b)(ctx); b.zero
  }

  protected def castLift[R:Num](x: Any)(implicit ctx: SrcCtx): R = {
    // Ideally we would want to search lift methods here so we can avoid code duplication?
    new UnsupportedLiftError(x, typ[R])(ctx)
    num[R].zero
  }
}