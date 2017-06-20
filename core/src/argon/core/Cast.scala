package argon.core

import forge.internal
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot find way to cast type ${A} to type ${B}.")
abstract class Cast[A,B](implicit mB: Type[B]) {
  val staged = mB
  @internal def apply(x: A): B
}