package argon.lang.direct

import argon.core._
import forge._

trait CastsApi {
  implicit class CastOps[A](x: A) {
    @api def to[B:Type](implicit cast: Cast[A,B]): B = cast(x)
  }
}