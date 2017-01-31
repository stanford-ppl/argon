package argon.ops

import argon.core.{Base, Staging}

/** Types which are represented by a static number of bits **/
trait BitsOps extends Base {
  type Bits[T] <: Staged[T]

  def zero[T:Bits](implicit ctx: SrcCtx): T
  def one[T:Bits](implicit ctx: SrcCtx): T
  def random[T:Bits](implicit ctx: SrcCtx): T
}
trait BitsApi extends BitsOps


trait BitsExp extends BitsOps with Staging {
  trait Bits[T] extends Staged[T] {
    def zero(implicit ctx: SrcCtx): T
    def one(implicit ctx: SrcCtx): T
    def random(max: Option[T])(implicit ctx: SrcCtx): T
    //def ones: T
    def length: Int
  }

  def zero[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].zero
  def one[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].one
  def random[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].random(None)
  def random[T:Bits](max: T)(implicit ctx: SrcCtx): T = implicitly[Bits[T]].random(Some(max))

  def bits[T:Bits] = implicitly[Bits[T]]
  def mbits[T,R](s: Staged[T]): Bits[R] = s.asInstanceOf[Bits[R]]
}

