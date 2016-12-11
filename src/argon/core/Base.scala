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

  /** Including evidence of Staged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the return type of lift depends on the second type parameter of Lift */
  @implicitNotFound(msg = "Cannot find way to lift type ${A}. Try adding explicit lift(_) calls to return value(s).")
  trait Lift[A,B] {
    def staged: Staged[B]
    def lift(x: A): B = __lift(x)(this)
  }

  private[argon] def __lift[A,B](x: A)(implicit l: Lift[A,B]): B
  final def lift[A,B](x: A)(implicit l: Lift[A,B]): B = l.lift(x)

  implicit def selfLift[T:Staged]: Lift[T,T] = new Lift[T,T] {
    def staged = implicitly[Staged[T]]
    override def lift(x: T): T = x
  }
}
