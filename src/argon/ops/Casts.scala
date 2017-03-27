package argon.ops

import argon.core.Staging
import argon.typeclasses._

trait CastApi extends CastExp {
  this: BoolExp with NumExp =>

  implicit class CastOps[A](x: A) {
    def to[B:Meta](implicit ctx: SrcCtx, cast: Cast[A,B]): B = cast(x)
  }

  implicit class LiftOps[A](x: A) {
    def as[B:Meta:Num](implicit ctx: SrcCtx, lift: Cast[A,B]): B = lift(x)
  }
}

trait CastExp extends Staging {
  this: BoolExp =>

}
