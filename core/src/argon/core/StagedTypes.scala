package argon.core

import argon._
import argon.lang.{Text,Bool}
import forge._

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait StagedTypes extends EmbeddedControls { this: ArgonCore =>

  implicit def subTypeEv[T:Type](x: T): MetaAny[T] = typ[T].ev(x)

  // Has to be an implicit class to not conflict with higher priority implicits on +
  implicit class ConcatOps[T<:MetaAny[T]](lhs: T) {
    @api def +(rhs: String): Text = lhs.toText + Text(rhs)
    @api def +(rhs: Text): Text = lhs.toText + rhs
    @api def +[R](rhs: MetaAny[R]): Text = lhs.toText + rhs.toText
  }

  @internal def infix_toString(x: MetaAny[_]): Text = x.toText

  @stateful def __valDef[T<:MetaAny[T]](init: T, name: String): Unit = {
    log(c"Setting name of ${init.s} to $name")
    init.s.name = Some(name)
  }

  @internal def __equals[T<:MetaAny[T]](x: T, y: T): Bool = x === y
  @internal def __equals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): Bool = lift(x) === y
  @internal def __equals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): Bool = x === lift(y)
  @internal def __unequals[T<:MetaAny[T]](x: T, y: T): Bool = x =!= y
  @internal def __unequals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): Bool = lift(x) =!= y
  @internal def __unequals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): Bool = x =!= lift(y)

  def typ[T:Type] = implicitly[Type[T]]
  def mtyp[A,B](x: Type[A]): Type[B] = x.asInstanceOf[Type[B]]

  def wrap[T:Type](s: Exp[T]): T = typ[T].wrapped(s)
  def wrap[T:Type](xs: List[Exp[T]]): List[T] = xs.map{t => typ[T].wrapped(t) }
  def wrap[T:Type](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => typ[T].wrapped(t) }

  def unwrap[T:Type](x: T): Exp[T] = typ[T].unwrapped(x)
  def unwrap[T:Type](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => typ[T].unwrapped(t) }
  def unwrap[T:Type](xs: List[T]): List[Exp[T]] = xs.map{t => typ[T].unwrapped(t) }

  import StagedTypes._
  // TODO: Should these lifts be casts?
  def infix_==[A,B](x1: MetaAny[A], x2: MetaAny[B]): Bool = macro equalImpl[Bool]
  def infix_==[A, B, C <:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalLiftRightImpl[Bool]
  def infix_==[A, B, C <:MetaAny[B]](x1: A, x2: MetaAny[B])(implicit l: Lift[A,C]): Bool = macro equalLiftLeftImpl[Bool]

  def infix_!=[A, B](x1: MetaAny[A], x2: MetaAny[B]): Bool = macro unequalImpl[Bool]
  def infix_!=[A, B, C<:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalLiftRightImpl[Bool]
  def infix_!=[A, B, C<:MetaAny[B]](x1:A, x2: MetaAny[B])(implicit l: Lift[A,C]): Bool = macro unequalLiftLeftImpl[Bool]


  // TODO: Should casts be implicit or explicit? Should have subtypes?

  @implicitNotFound(msg = "Cannot find way to cast type ${A} to type ${B}.")
  abstract class Cast[A,B](implicit mB: Type[B]) {
    val staged = mB
    @internal def apply(x: A): B
  }

  @internal final def cast[A,B](x: A)(implicit c: Cast[A,B]): B = c(x)

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

  @internal final def lift[A,B](x: A)(implicit l: Lift[A,B]): B = l(x)

  implicit def selfLift[T:Type]: Lift[T,T] = new Lift[T,T] {
    @internal override def apply(x: T): T = x
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
