package argon.ops

import argon.core.Core

trait NumCore extends Core with CustomBitWidths {
  abstract class Num[T<:Sym] extends Typ[T] {
    def zero: T
    def one: T
    //def ones: T
  }
  def num[T<:Sym:Num] = implicitly[Num[T]]

  case class NumRandom[T<:Sym:Num](t: Num[T]) extends Op[T] { def mirror(f: Tx) = random[T]() }

  def random[T<:Sym:Num]()(implicit ctx: SrcCtx): T = stageSimple( NumRandom[T](num[T]) )(ctx)
}

trait NumAPI extends NumCore
