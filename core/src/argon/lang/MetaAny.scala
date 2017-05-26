package argon.lang

import argon.core.Globals
import argon.core.compiler._
import forge._
import org.virtualized.EmbeddedControls

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/** Base trait for all staged, frontend types **/
abstract class MetaAny[T:Type] extends Product {
  type Internal
  def s: Exp[T]
  final def getOrElseCreate(func: => T): T = if (s == null) func else this.asInstanceOf[T]

  private def isEqual(that: Any): CBoolean = that match {
    case x: MetaAny[_] => this.s == x.s
    case _ => false
  }

  /*private def unstagedWarning(op: String)(implicit ctx: SrcCtx): Unit = {
    warn(s"Unstaged method $op was used here on a staged type during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }*/
  private def unstagedWarningNoCtx(op: CString): CUnit = {
    val name = s.name.getOrElse(s.toString)
    warn(s"Unstaged method $op was used on value $name during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
  }

  override def toString: CString = {
    if (Globals.staging) unstagedWarningNoCtx("toString")
    this.productPrefix + this.productIterator.mkString("(", ", ", ")")
  }

  override def equals(that: Any): CBoolean = {
    if (Globals.staging) unstagedWarningNoCtx("equals")
    this.isEqual(that)
  }

  @api def !=(that: T): MBoolean = this =!= that
  @api def ==(that: T): MBoolean = this === that
  @api def ===(that: T): MBoolean
  @api def =!=(that: T): MBoolean
  @api def toText: MString
}

trait MetaAnyLowPriorityImplicits {
  // Has to be an implicit class to not conflict with higher priority implicits on +
  class ConcatOps(lhs: MetaAny[_]) {
    @api def +(rhs: CString): MString = lhs.toText + String(rhs)
    @api def +(rhs: MString): MString = lhs.toText + rhs
    //@api def +(rhs: MetaAny[_]): MString = lhs.toText + rhs.toText
  }
  implicit def concatOps(lhs: MetaAny[_]): ConcatOps = new ConcatOps(lhs)
}

trait MetaAnyExp { }

trait MetaAnyApi extends EmbeddedControls {

  @internal def infix_toString(x: MetaAny[_]): MString = x.toText

  def __valDef[T<:MetaAny[T]](init: T, name: CString): CUnit = { init.s.name = Some(name) }

  // TODO: Should casts be implicit or explicit? Should have subtypes?
  @internal def __equals[T<:MetaAny[T]](x: T, y: T): MBoolean = x === y
  @internal def __equals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): MBoolean = lift(x) === y
  @internal def __equals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): MBoolean = x === lift(y)
  @internal def __unequals[T<:MetaAny[T]](x: T, y: T): MBoolean = x =!= y
  @internal def __unequals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): MBoolean = lift(x) =!= y
  @internal def __unequals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): MBoolean = x =!= lift(y)

  import StagedTypes._
  // TODO: Should these lifts be casts?
  def infix_==[A,B](x1: MetaAny[A], x2: MetaAny[B]): MBoolean = macro equalImpl[MBoolean]
  def infix_==[A, B, C <:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro equalLiftRightImpl[MBoolean]
  def infix_==[A, B, C <:MetaAny[B]](x1: A, x2: MetaAny[B])(implicit l: Lift[A,C]): MBoolean = macro equalLiftLeftImpl[MBoolean]

  def infix_!=[A, B](x1: MetaAny[A], x2: MetaAny[B]): MBoolean = macro unequalImpl[MBoolean]
  def infix_!=[A, B, C<:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro unequalLiftRightImpl[MBoolean]
  def infix_!=[A, B, C<:MetaAny[B]](x1:A, x2: MetaAny[B])(implicit l: Lift[A,C]): MBoolean = macro unequalLiftLeftImpl[MBoolean]
}


private object StagedTypes {
  def equalImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === $x2")
  }

  def equalLiftRightImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === lift($x2)")
  }

  def equalLiftLeftImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === $x2")
  }

  def unequalImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"__unequals($x1, $x2)")
  }

  def unequalLiftRightImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 =!= lift($x2)")
  }

  def unequalLiftLeftImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) =!= $x2")
  }
}
