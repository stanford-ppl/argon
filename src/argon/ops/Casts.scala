package argon.ops

import argon.core.Staging
import argon.typeclasses._
import org.virtualized.stageany


trait CastApi extends CastExp with NumApi {
  this: BoolExp =>

}


@stageany
trait CastExp extends Staging with NumExp {
  this: BoolExp =>

  implicit class CastOps[T: Staged:Num](x: T) {
    def to[R:StageAny:Num](implicit ctx: SrcCtx): R = cast[T,R](x)
  }

  implicit class ConstCastOps[T:Numeric](x: T) {
    def as[R:StageAny:Num](implicit ctx: SrcCtx): R = castLift[R](x)
  }

  protected def cast[T: Staged:Num,R:StageAny:Num](x: T)(implicit ctx: SrcCtx): R = (num[T],num[R]) match {
    case (a,b) if a == b => x.asInstanceOf[R]
    case _ =>
      new UnsupportedCastError(ftyp[T],ftyp[R])(ctx)
      wrap(fresh[R])
  }

  protected def castLift[R:StageAny:Num](x: Any)(implicit ctx: SrcCtx): R = {
    // Ideally we would want to search lift methods here so we can avoid code duplication?
    new UnsupportedLiftError(x, ftyp[R])(ctx)
    wrap(fresh[R])
  }

}
