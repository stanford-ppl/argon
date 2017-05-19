package argon.core
package cake

import argon.lang.MetaAny
import forge._

import scala.annotation.implicitNotFound

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


