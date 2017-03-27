package argon.typeclasses

import argon.ops.{BoolExp, CastExp, FixPtExp, FltPtExp}

trait NumApi extends NumExp { this: BoolExp with FixPtExp with FltPtExp with CastExp => }
trait NumExp extends ArithExp with BitsExp with OrderExp {
  this: BoolExp with FixPtExp with FltPtExp with CastExp =>

  trait Num[T] extends Bits[T] with Arith[T] with Order[T] {
    def toFixPt[S:BOOL,I:INT,F:INT](x: T)(implicit ctx: SrcCtx): FixPt[S,I,F]
    def toFltPt[G:INT,E:INT](x: T)(implicit ctx: SrcCtx): FltPt[G,E]
  }

  implicit def num2fltpt[T:Num,G:INT,E:INT] = new Cast[T,FltPt[G,E]] {
    def apply(x: T)(implicit ctx: SrcCtx) = implicitly[Num[T]].toFltPt[G,E](x)
  }
  implicit def num2fixpt[T:Num,S:BOOL,I:INT,F:INT] = new Cast[T,FixPt[S,I,F]] {
    def apply(x: T)(implicit ctx: SrcCtx) = implicitly[Num[T]].toFixPt[S,I,F](x)
  }

  def num[T:Num] = implicitly[Num[T]]
}
