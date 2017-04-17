package argon.typeclasses

import argon._
import argon.ops._

trait NumApi extends NumExp { self: ArgonApi => }

trait LowPriorityNumImplicits { self: ArgonApi =>

  // FIXME: Users may want to write x.to[T], where T is a generic type with evidence of Num
  // Should this be allowed? Better way to support in general?
  implicit def num2num[T:Meta:Num,R:Meta:Num] = new Cast[T,R] {
    def apply(x: T)(implicit ctx: SrcCtx): R = typ[R] match {
      case tp: FixPtType[s,i,f] => num[T].toFixPt(x)(tp.mS, tp.mI, tp.mF, ctx).asInstanceOf[R]
      case tp: FltPtType[g,e]   => num[T].toFltPt(x)(tp.mG, tp.mE, ctx).asInstanceOf[R]
      case _ => throw new Exception(u"Cannot find way to convert type ")
    }
  }

}

trait NumExp { self: ArgonExp =>

  trait Num[T] extends Bits[T] with Arith[T] with Order[T] {
    def toFixPt[S:BOOL,I:INT,F:INT](x: T)(implicit ctx: SrcCtx): FixPt[S,I,F]
    def toFltPt[G:INT,E:INT](x: T)(implicit ctx: SrcCtx): FltPt[G,E]

    def fromInt(x: Int, force: Boolean = true)(implicit ctx: SrcCtx): T
    def fromLong(x: Long, force: Boolean = true)(implicit ctx: SrcCtx): T
    def fromFloat(x: Float, force: Boolean = true)(implicit ctx: SrcCtx): T
    def fromDouble(x: Double, force: Boolean = true)(implicit ctx: SrcCtx): T
  }

  def num[T:Num]: Num[T] = implicitly[Num[T]]

  implicit def num2fltpt[T:Num,G:INT,E:INT] = new Cast[T,FltPt[G,E]] {
    def apply(x: T)(implicit ctx: SrcCtx): FltPt[G,E] = num[T].toFltPt[G,E](x)
  }
  implicit def num2fixpt[T:Num,S:BOOL,I:INT,F:INT] = new Cast[T,FixPt[S,I,F]] {
    def apply(x: T)(implicit ctx: SrcCtx): FixPt[S,I,F] = num[T].toFixPt[S,I,F](x)
  }

  implicit def int2numT[T:Meta:Num] = new Cast[Int,T] {
    def apply(x: Int)(implicit ctx: SrcCtx): T = num[T].fromInt(x)
  }
  implicit def long2numT[T:Meta:Num] = new Cast[Long,T] {
    def apply(x: Long)(implicit ctx: SrcCtx): T = num[T].fromLong(x)
  }
  implicit def float2numT[T:Meta:Num] = new Cast[Float,T] {
    def apply(x: Float)(implicit ctx: SrcCtx): T = num[T].fromFloat(x)
  }
  implicit def double2numT[T:Meta:Num] = new Cast[Double,T] {
    def apply(x: Double)(implicit ctx: SrcCtx): T = num[T].fromDouble(x)
  }

}
