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
    def random(implicit ctx: SrcCtx): T
    //def ones: T
  }

  def zero[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].zero
  def one[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].one
  def random[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].random

  def bits[T:Bits] = implicitly[Bits[T]]
}

