package argon.ops

import argon.core.{Base, Core}

trait NumOps extends Base {
  abstract class Num[T] extends Typ[T] {
    def zero: T
    def one: T
    def random(implicit ctx: SrcCtx): T
    //def ones: T
  }

  def zero[T:Num]: T = implicitly[Num[T]].zero
  def one[T:Num]: T = implicitly[Num[T]].one
  def random[T:Num](implicit ctx: SrcCtx): T = implicitly[Num[T]].random
}
trait NumAPI extends NumOps


trait NumCore extends NumOps with Core {
  def num[T:Num] = implicitly[Num[T]]
}

