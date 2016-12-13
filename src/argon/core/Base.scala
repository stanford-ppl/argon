package argon.core

import argon.State

import scala.annotation.implicitNotFound
import scala.virtualized.{EmbeddedControls, SourceContext}

trait Base extends EmbeddedControls with Reporting {
  type SrcCtx = SourceContext
  type Staged[T]

  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }

  /** Lift[A,B] is used in place of Staged[T] for return types of user facing blocks, where the user may either
    * give an unstaged constant or a staged symbol as the return value.
    *
    * NOTE: Including evidence of Staged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve Staged[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result Staged[_] in scope to qualify.
    **/

  @implicitNotFound(msg = "Cannot find way to lift type ${A}. Try adding explicit lift(_) calls to return value(s).")
  trait Lift[A,B] {
    def staged: Staged[B]
    def lift(x: A)(implicit ctx: SrcCtx): B = __lift(x)(ctx, this)
  }

  def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B
  final def lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.lift(x)

  implicit def selfLift[T:Staged]: Lift[T,T] = new Lift[T,T] {
    def staged = implicitly[Staged[T]]
    override def lift(x: T)(implicit ctx: SrcCtx): T = x
  }
}
