package argon.core

import scala.annotation.implicitNotFound

import org.virtualized.{EmbeddedControls, SourceContext}
import argon.State

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext

  /** Base type class for all staged types **/
  abstract class Staged[T] {
    def wrapped(x: Exp[T]): T
    def unwrapped(x: T): Exp[T]
    def typeArguments: List[Staged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean

    def <:<(that: Staged[_]) = isSubtype(this.stagedClass, that.stagedClass)
  }

  def typ[T:Staged] = implicitly[Staged[T]]
  def mtyp[A,B](x: Staged[A]): Staged[B] = x.asInstanceOf[Staged[B]]

  def wrap[T:Staged](s: Exp[T]): T = implicitly[Staged[T]].wrapped(s)
  def unwrap[T:Staged](x: T): Exp[T] = implicitly[Staged[T]].unwrapped(x)

  def wrap[T:Staged](xs: List[Exp[T]]): List[T] = xs.map{t => implicitly[Staged[T]].wrapped(t) }
  def unwrap[T:Staged](xs: List[T]): List[Exp[T]] = xs.map{t => implicitly[Staged[T]].unwrapped(t) }
  def wrap[T:Staged](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => implicitly[Staged[T]].wrapped(t) }
  def unwrap[T:Staged](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => implicitly[Staged[T]].unwrapped(t) }

  implicit class StagedTypeOps[T:Staged](x: T) {
    def s: Exp[T] = implicitly[Staged[T]].unwrapped(x)
  }

  /** Stolen from Delite utils **/
  private def isSubtype(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
    if ((x == cls) || x.getInterfaces.contains(cls)) true
    else if (x.getSuperclass == null && x.getInterfaces.length == 0) false
    else {
      val superIsSub = if (x.getSuperclass != null) isSubtype(x.getSuperclass, cls) else false
      superIsSub || x.getInterfaces.exists(s=>isSubtype(s,cls))
    }
  }


  /** Lift[A,B] is used in place of Staged[T] for user-facing type parameters, where the user may either
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



  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}
