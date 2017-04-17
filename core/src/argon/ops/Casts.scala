package argon.ops

import argon.core.Staging
import argon.typeclasses._
import forge._

trait CastApi extends CastExp { this: BoolExp with NumExp =>

  implicit class CastOps[A](x: A) {
    @api def to[B: Meta](implicit cast: Cast[A, B]): B = cast(x)
  }

  /*implicit class LiftOps[A](x: A) {
    @api def as[B:Meta](implicit cast: Cast[A,B]): B = {
      log(c"Called .to[${typ[B]}] on $x")
      cast(x)
    }
  }*/
}

trait CastExp extends Staging { this: BoolExp =>

}
