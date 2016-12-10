package argon.ops

import argon.core.{Base, Staging}

/** Types which are represented by a static number of bits **/
trait BitsOps extends Base {
  type Bits[T] <: Staged[T]

  def zero[T:Bits]: T
  def one[T:Bits]: T
  def random[T:Bits](implicit ctx: SrcCtx): T
}
trait BitsApi extends BitsOps


trait BitsExp extends BitsOps with Staging {
  abstract class Bits[T] extends Staged[T] {
    def zero: T
    def one: T
    def random(implicit ctx: SrcCtx): T
    //def ones: T
  }

  def zero[T:Bits]: T = implicitly[Bits[T]].zero
  def one[T:Bits]: T = implicitly[Bits[T]].one
  def random[T:Bits](implicit ctx: SrcCtx): T = implicitly[Bits[T]].random

  def bits[T:Bits] = implicitly[Bits[T]]
}

