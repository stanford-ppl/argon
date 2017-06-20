package argon.core

import forge.internal
import scala.annotation.implicitNotFound

/** Lift[A,B] is used in place of Type[T] for user-facing type parameters, where the user may either
  * give an unstaged constant or a staged symbol as the return value.
  *
  * NOTE: Including evidence of Type[B] as an implicit parameter to Lift instances leads to problems with implicit
  * ambiguity when calling lift(x), since the compiler may attempt to resolve Type[B] before it resolves Lift[A,B],
  * causing any implicit value or def with result Type[_] in scope to qualify.
  **/
@implicitNotFound(msg = "Cannot find way to lift type ${A} to type ${B}. Try adding an explicit cast using .to[${B}].")
abstract class Lift[A,B](implicit mB: Type[B]) {
  val staged = mB
  @internal def apply(x: A): B
}