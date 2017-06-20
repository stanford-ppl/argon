package argon.lang.typeclasses

import argon.core._
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

