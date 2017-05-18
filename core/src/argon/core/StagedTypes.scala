package argon.core

import argon._
import forge._
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Cannot find way to cast type ${A} to type ${B}.")
abstract class Cast[A,B](implicit mB: Type[B]) {
  val staged = mB
  @internal def apply(x: A): B
}

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


trait StagedTypes { this: ArgonCore =>
  implicit def subTypeEv[T:Type](x: T): MetaAny[T] = typ[T].ev(x)

  def typ[T:Type] = implicitly[Type[T]]
  def mtyp[A,B](x: Type[A]): Type[B] = x.asInstanceOf[Type[B]]

  def wrap[T:Type](s: Exp[T]): T = typ[T].wrapped(s)
  def wrap[T:Type](xs: List[Exp[T]]): List[T] = xs.map{t => typ[T].wrapped(t) }
  def wrap[T:Type](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => typ[T].wrapped(t) }

  def unwrap[T:Type](x: T): Exp[T] = typ[T].unwrapped(x)
  def unwrap[T:Type](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => typ[T].unwrapped(t) }
  def unwrap[T:Type](xs: List[T]): List[Exp[T]] = xs.map{t => typ[T].unwrapped(t) }

  @internal final def cast[A,B](x: A)(implicit c: Cast[A,B]): B = c(x)
  @internal final def lift[A,B](x: A)(implicit l: Lift[A,B]): B = l(x)

  implicit def selfLift[T:Type]: Lift[T,T] = new Lift[T,T] {
    @internal override def apply(x: T): T = x
  }
}


