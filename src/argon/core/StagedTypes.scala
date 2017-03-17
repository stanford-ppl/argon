package argon.core

import scala.annotation.implicitNotFound

import org.virtualized.{EmbeddedControls, SourceContext}
import argon.State

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext

  /** Base type class for all FStaged types **/
  trait FStaged[T] extends BStaged[T]{
    def wrapped(x: Exp[T]): T
    def unwrapped(x: T): Exp[T]
  }

  trait BStaged[T] {
    def typeArguments: List[BStaged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
    def <:<(that: FStaged[_]) = isSubtype(this.stagedClass, that.stagedClass)
  }

  def ftyp[T:FStaged] = implicitly[FStaged[T]]
  def btyp[T:BStaged] = implicitly[BStaged[T]]
  def mftyp[A,B](x: FStaged[A]): FStaged[B] = x.asInstanceOf[FStaged[B]]
  def mbtyp[A,B](x: BStaged[A]): BStaged[B] = x.asInstanceOf[BStaged[B]]

  def wrap[T:FStaged](s: Exp[T]): T = implicitly[FStaged[T]].wrapped(s)
  def unwrap[T:FStaged](x: T): Exp[T] = implicitly[FStaged[T]].unwrapped(x)

  def wrap[T:FStaged](xs: List[Exp[T]]): List[T] = xs.map{t => implicitly[FStaged[T]].wrapped(t) }
  def unwrap[T:FStaged](xs: List[T]): List[Exp[T]] = xs.map{t => implicitly[FStaged[T]].unwrapped(t) }
  def wrap[T:FStaged](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => implicitly[FStaged[T]].wrapped(t) }
  def unwrap[T:FStaged](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => implicitly[FStaged[T]].unwrapped(t) }

  implicit class FStagedTypeOps[T:FStaged](x: T) {
    def s: Exp[T] = implicitly[FStaged[T]].unwrapped(x)
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


  /** Lift[A,B] is used in place of FStaged[T] for user-facing type parameters, where the user may either
    * give an unFStaged constant or a FStaged symbol as the return value.
    *
    * NOTE: Including evidence of FStaged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve FStaged[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result FStaged[_] in scope to qualify.
    **/

  @implicitNotFound(msg = "Cannot find way to lift type ${A}. Try adding explicit lift(_) calls to return value(s).")
  trait Lift[A,B] {
    def FStaged: FStaged[B]
    def lift(x: A)(implicit ctx: SrcCtx): B = __lift(x)(ctx, this)
  }

  def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B
  final def lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.lift(x)

  implicit def selfLift[T:FStaged]: Lift[T,T] = new Lift[T,T] {
    def FStaged = implicitly[FStaged[T]]
    override def lift(x: T)(implicit ctx: SrcCtx): T = x
  }



  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}
