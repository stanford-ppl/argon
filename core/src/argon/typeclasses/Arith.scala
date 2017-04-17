package argon.typeclasses

import argon.{ArgonApi, ArgonExp}
import argon.core.{StagedTypes, Staging}

trait ArithApi extends ArithExp { self: ArgonApi => }

trait ArithExp extends StagedTypes { self: ArgonExp =>

  trait Arith[T] {
    def negate(x: T)(implicit ctx: SrcCtx): T
    def plus(x: T, y: T)(implicit ctx: SrcCtx): T
    def minus(x: T, y: T)(implicit ctx: SrcCtx): T
    def times(x: T, y: T)(implicit ctx: SrcCtx): T
    def divide(x: T, y: T)(implicit ctx: SrcCtx): T
  }

  implicit class ArithOps[T:Arith](lhs: T) {
    def unary_-(implicit ctx: SrcCtx): T = arith[T].negate(lhs)
    def +(rhs: T)(implicit ctx: SrcCtx): T = arith[T].plus(lhs, rhs)
    def -(rhs: T)(implicit ctx: SrcCtx): T = arith[T].minus(lhs, rhs)
    def *(rhs: T)(implicit ctx: SrcCtx): T = arith[T].times(lhs, rhs)
    def /(rhs: T)(implicit ctx: SrcCtx): T = arith[T].divide(lhs, rhs)
  }

  def arith[T:Arith] = implicitly[Arith[T]]
}

