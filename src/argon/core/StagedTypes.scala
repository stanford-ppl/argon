package argon.core

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext, stageany}
import argon.State

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@stageany
trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext

  trait Staged[T] {
    def wrapped(e: Exp[T]): T
    def typeArguments: List[Staged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
    def <:<(that: Staged[_]) = isSubtype(this.stagedClass, that.stagedClass)
  }

  type FStaged[T <: StageAny[T]] = Staged[T]

  type Bool
  type Text

  trait StageAny[T] {
    def s: Exp[T]
    def ===(x: T)(implicit ctx: SrcCtx): Bool
    def =!=(x: T)(implicit ctx: SrcCtx): Bool
    def toText(implicit ctx: SrcCtx): Text
  }

  def infix_toString[T:StageAny](x: T) = x.toText


  def __equals[T:StageAny](x: T, y: T)(implicit ctx: SrcCtx): Bool = {
    x === y
  }

  def __equals[A, T:StageAny](x: A, y: T)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = {
    l.lift(x) === y
  }

  def __equals[A, T:StageAny](x: T, y: A)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = {
    x === l.lift(y)
  }

  def __unequals[T:StageAny](x: T, y: T)(implicit ctx: SrcCtx): Bool = {
    x =!= y
  }

  def __unequals[A, T:StageAny](x: A, y: T)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = {
    l.lift(x) =!= y
  }

  def __unequals[A, T:StageAny](x: T, y: A)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = {
    x =!= l.lift(y)
  }

  import StagedTypes._


  def infix_==[A,B](x1: StageAny[A], x2: StageAny[B]): Bool = macro equalImpl[Bool]
  def infix_==[A, B, C <: StageAny[B]](x1: StageAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalLiftRightImpl[Bool]
  def infix_==[A, B, C <: StageAny[B]](x1: A, x2: StageAny[B])(implicit l: Lift[A,C]): Bool = macro equalLiftLeftImpl[Bool]

  def infix_!=[A, B](x1: StageAny[A], x2: StageAny[B]): Bool = macro unequalImpl[Bool]
  def infix_!=[A, B, C <: StageAny[B]](x1: StageAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalLiftRightImpl[Bool]
  def infix_!=[A, B, C <: StageAny[B]](x1:A, x2: StageAny[B])(implicit l: Lift[A,C]): Bool = macro unequalLiftLeftImpl[Bool]

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
  trait Lift[A,B] {
    val staged: Staged[B]
    def lift(x: A)(implicit ctx: SrcCtx): B
  }

  final def lift[A, B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.lift(x)

  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}

private object StagedTypes {



  def equalImpl[T](c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === $x2")
  }

  def equalLiftRightImpl[T](c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === lift($x2)")
  }

  def equalLiftLeftImpl[T](c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === $x2")
  }

  def unequalImpl[T](c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"__unequals($x1, $x2)")
  }

  def unequalLiftRightImpl[T](c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === lift($x2)")
  }

  def unequalLiftLeftImpl[T](c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === $x2")
  }
}
