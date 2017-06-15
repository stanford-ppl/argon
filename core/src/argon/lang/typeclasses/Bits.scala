package argon.lang.typeclasses

import argon.internals._
import forge._

/** Types which are represented by a static number of bits **/
trait Bits[T] {
  @api def zero: T
  @api def one: T
  @api def random(max: Option[T]): T
  def length: Int
}

trait CanBits[T] { this: Type[T] =>
  final def getBits: Option[Bits[T]] = getBits(this.typeArguments)
  protected def getBits(children: Seq[Type[_]]): Option[Bits[T]]
}

object Bits {
  def unapply[T](x: Type[T]): Option[Bits[T]] = x match {
    case lookup: CanBits[_] => lookup.getBits.asInstanceOf[Option[Bits[T]]]
    case _ => None
  }
}

trait BitsExp {
  def bits[T:Bits] = implicitly[Bits[T]]
  def mbits[T,R](s: Bits[T]): Bits[R] = s.asInstanceOf[Bits[R]]
}

trait BitsApi {
  @api def zero[T:Bits]: T = implicitly[Bits[T]].zero
  @api def one[T:Bits]: T = implicitly[Bits[T]].one
  @api def random[T:Bits]: T = implicitly[Bits[T]].random(None)
  @api def random[T:Bits](max: T): T = implicitly[Bits[T]].random(Some(max))
}

