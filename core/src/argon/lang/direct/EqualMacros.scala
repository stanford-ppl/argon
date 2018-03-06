package argon.lang.direct

import argon.core._
import virtualized.EmbeddedControls

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/** MetaAny and Var
  * (Doesn't matter if Var is actually supported in the DSL or not - will never be used if it isn't)
  */
// TODO: Why does EmbeddedControls have to be mixed in here?
trait EqualMacros extends EmbeddedControls {
  import StagedVariables._
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): MBoolean = macro equalVarLeft[MBoolean]
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): MBoolean = macro equalVarRight[MBoolean]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro equalVarLiftRight[MBoolean]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): MBoolean = macro equalVarLiftLeft[MBoolean]

  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): MBoolean = macro unequalVarLeft[MBoolean]
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): MBoolean = macro unequalVarRight[MBoolean]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro unequalVarLiftRight[MBoolean]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): MBoolean = macro unequalVarLiftLeft[MBoolean]

  import StagedTypes._
  def infix_==[A,B](x1: MetaAny[A], x2: MetaAny[B]): MBoolean = macro equalImpl[MBoolean]
  def infix_==[A, B, C <:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro equalLiftRightImpl[MBoolean]
  def infix_==[A, B, C <:MetaAny[B]](x1: A, x2: MetaAny[B])(implicit l: Lift[A,C]): MBoolean = macro equalLiftLeftImpl[MBoolean]

  def infix_!=[A, B](x1: MetaAny[A], x2: MetaAny[B]): MBoolean = macro unequalImpl[MBoolean]
  def infix_!=[A, B, C<:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro unequalLiftRightImpl[MBoolean]
  def infix_!=[A, B, C<:MetaAny[B]](x1:A, x2: MetaAny[B])(implicit l: Lift[A,C]): MBoolean = macro unequalLiftLeftImpl[MBoolean]
}


private object StagedVariables {
  def equalVarLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) === $x2")
  }
  def equalVarRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"x1 === readVar($x2)")
  }

  def equalVarLiftRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) === lift($x2)")
  }

  def equalVarLiftLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === readVar($x2)")
  }

  def unequalVarLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) =!= $x2")
  }

  def unequalVarRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 =!= readVar($x2)")
  }

  def unequalVarLiftRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) =!= lift($x2)")
  }

  def unequalVarLiftLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) =!= readVar($x2)")
  }
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