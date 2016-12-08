package argon.ops

import argon.core.{Base, Staging}

/** Types which are represented by a static number of bits **/
trait Bits extends Base {
  type Bit[T] <: Staged[T]

  def zero[T:Bit]: T
  def one[T:Bit]: T
  def random[T:Bit](implicit ctx: SrcCtx): T
}
trait BitAPI extends Bits


trait BitExp extends Bits with Staging {
  abstract class Bit[T] extends Staged[T] {
    def zero: T
    def one: T
    def random(implicit ctx: SrcCtx): T
    //def ones: T
  }

  def zero[T:Bit]: T = implicitly[Bit[T]].zero
  def one[T:Bit]: T = implicitly[Bit[T]].one
  def random[T:Bit](implicit ctx: SrcCtx): T = implicitly[Bit[T]].random

  def bits[T:Bit] = implicitly[Bit[T]]
}

