package argon.typeclasses

import argon.core.Staging

/** Types which are represented by a static number of bits **/
trait BitsApi extends BitsExp {

}

trait BitsExp extends Staging {

  trait Bits[T] {
    def zero(implicit ctx: SrcCtx): T
    def one(implicit ctx: SrcCtx): T
    def random(max: Option[T])(implicit ctx: SrcCtx): T
    def length: Int
  }

  protected def bitsUnapply[T](tp: Staged[T]): Option[Bits[T]] = None

  object Bits {
    def unapply[T](x: Staged[T]): Option[Bits[T]] = bitsUnapply(x)
  }

  def zero[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].zero
  def one[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].one
  def random[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].random(None)
  def random[T:Bits](max: T)(implicit ctx: SrcCtx): T = implicitly[Bits[T]].random(Some(max))

  def bits[T:Bits] = implicitly[Bits[T]]
  def mbits[T,R](s: Bits[T]): Bits[R] = s.asInstanceOf[Bits[R]]
}

