package argon.typeclasses

import argon.{ArgonApi, ArgonExp}
import argon.core.Staging

/** Types which are represented by a static number of bits **/
trait BitsApi extends BitsExp {
  self: ArgonApi =>

}

trait BitsExp {
  self: ArgonExp =>

  trait Bits[T] {
    def zero(implicit ctx: SrcCtx): T
    def one(implicit ctx: SrcCtx): T
    def random(max: Option[T])(implicit ctx: SrcCtx): T
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

  def zero[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].zero
  def one[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].one
  def random[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].random(None)
  def random[T:Bits](max: T)(implicit ctx: SrcCtx): T = implicitly[Bits[T]].random(Some(max))

  def bits[T:Bits] = implicitly[Bits[T]]
  def mbits[T,R](s: Bits[T]): Bits[R] = s.asInstanceOf[Bits[R]]
}

