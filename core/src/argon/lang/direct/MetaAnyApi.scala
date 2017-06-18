package argon.lang.direct

import argon.core._
import forge._
import org.virtualized.EmbeddedControls

import scala.reflect.macros.whitebox
import scala.language.experimental.macros


trait MetaAnyLowPriorityImplicits {
  // Has to be an implicit class to not conflict with higher priority implicits on +
  implicit class ConcatOps(lhs: MetaAny[_]) {
    @api def +(rhs: CString): MString = lhs.toText + MString(rhs)
    @api def +(rhs: MString): MString = lhs.toText + rhs
    //@api def +(rhs: MetaAny[_]): MString = lhs.toText + rhs.toText
  }
}

trait MetaAnyExp extends EmbeddedControls with MetaAnyLowPriorityImplicits {

  @internal def infix_toString(x: MetaAny[_]): MString = x.toText

  def __valDef[T<:MetaAny[T]](init: T, name: CString): CUnit = { init.s.name = Some(name) }

  // TODO: Should casts be implicit or explicit? Should have subtypes?
  @internal def __equals[T<:MetaAny[T]](x: T, y: T): MBoolean = x === y
  @internal def __equals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): MBoolean = lift(x) === y
  @internal def __eMetaAnyquals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): MBoolean = x === lift(y)
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