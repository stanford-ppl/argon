package argon.core

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}
import argon.State
import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext

  /** Base type class for all Staged types **/
  trait Staged[T] {
    def wrapped(e: Exp[T]): T
    def typeArguments: List[Staged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
    def <:<(that: Staged[_]) = isSubtype(this.stagedClass, that.stagedClass)
  }

  type FStaged[T <: StageAny[T]] = Staged[T]

  trait StageAny[T] {
    def s: Exp[T]
    def ===(x: T): Boolean
    def =!=(x: T): Boolean
  }

  import StagedTypes._

  def infix_==[A,B](x1: StageAny[A], x2: StageAny[B]): Boolean = macro incorrectEqualImpl
  def infix_==[A](x1: StageAny[A], x2: StageAny[A]): Boolean = macro correctEqualImpl

  def infix_!=[A,B](x1: StageAny[A], x2: StageAny[B]): Boolean = macro incorrectUnequalImpl
  def infix_!=[A](x1: StageAny[A], x2: StageAny[A]): Boolean = macro correctUnequalImpl

  def ftyp[T: Staged]: Staged[T] = implicitly[Staged[T]]
  def btyp[T: Staged] = implicitly[Staged[T]]
  def mbtyp[A,B](x: Staged[A]): Staged[B] = x.asInstanceOf[Staged[B]]
  def mftyp[A,B](x: Staged[A]): Staged[B] = x.asInstanceOf[Staged[B]]

  def wrap[T: Staged](s: Exp[T]): T = implicitly[Staged[T]].wrapped(s)
  def wrap[T: Staged](xs: List[Exp[T]]): List[T] = xs.map{t => implicitly[Staged[T]].wrapped(t) }
  def wrap[T: Staged](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => implicitly[Staged[T]].wrapped(t) }


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
    * give an unStaged constant or a Staged symbol as the return value.
    *
    * NOTE: Including evidence of Staged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve Staged[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result Staged[_] in scope to qualify.
    **/

  @implicitNotFound(msg = "Cannot find way to lift type ${A}. Try adding explicit lift(_) calls to return value(s).")
  trait Lift[A,B <: StageAny[B]] {
    def Staged: Staged[B]
    def lift(x: A)(implicit ctx: SrcCtx): B = __lift(x)(ctx, this)
  }

  def __lift[A,B <: StageAny[B]](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B
  final def lift[A,B <: StageAny[B]](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.lift(x)

  implicit def selfLift[T <: StageAny[T] : FStaged]: Lift[T,T] = new Lift[T,T] {
    def Staged = implicitly[Staged[T]]
    override def lift(x: T)(implicit ctx: SrcCtx): T = x
  }



  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}

private object StagedTypes {

  def incorrectEqualImpl(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Boolean] = {
    c.abort(c.enclosingPosition, "Should compare similar type of Stage Any")
  }

  def correctEqualImpl(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Boolean] = {
    import c.universe._
    c.Expr(q"$x1 === $x2")
  }

  def incorrectUnequalImpl(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Boolean] = {
    c.abort(c.enclosingPosition, "Should compare similar type of Stage Any")
  }

  def correctUnequalImpl(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Boolean] = {
    import c.universe._
    c.Expr(q"$x1 =!= $x2")
  }
}
