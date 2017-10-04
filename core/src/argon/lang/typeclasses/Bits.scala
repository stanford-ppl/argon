package argon.lang.typeclasses

import argon.core._
import forge._

/** Types which are represented by a static number of bits **/
trait Bits[T] {
  /** Returns the minimum number of bits required to represent type T. **/
  def length: Int

  /** Returns the zero value for type T. **/
  @api def zero: T
  /** Returns the one value for type T. **/
  @api def one: T

  /**
    * Generates a pseudorandom value uniformly distributed between 0 and max.
    * If max is unspecified, type T's default maximum is used instead.
    * For @FixPt types, the default maximum is the maximum representable number.
    * For @FltPt types, the default maximum is 1.
    * For composite @Tuple2 and @Struct types, the maximum is determined per component.
    **/
  @api def random(max: Option[T]): T
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

