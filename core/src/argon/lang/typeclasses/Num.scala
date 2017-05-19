package argon.lang.typeclasses

import argon.lang.{FixPt,FltPt}
import argon.compiler._
import argon.nodes._
import forge._

trait Num[T] extends Bits[T] with Arith[T] with Order[T] {
  @api def toFixPt[S:BOOL,I:INT,F:INT](x: T): FixPt[S,I,F]
  @api def toFltPt[G:INT,E:INT](x: T): FltPt[G,E]

  @api def fromInt(x: Int, force: Boolean = true): T
  @api def fromLong(x: Long, force: Boolean = true): T
  @api def fromFloat(x: Float, force: Boolean = true): T
  @api def fromDouble(x: Double, force: Boolean = true): T
}


trait LowPriorityNumImplicits { self: NumExp =>
  // FIXME: Users may want to write x.to[T], where T is a generic type with evidence of Num
  // Should this be allowed? Better way to support in general?
  implicit def num2num[T:Type:Num,R:Type:Num] = new Cast[T,R] {
    @internal def apply(x: T): R = typ[R] match {
      case tp: FixPtType[s,i,f] =>
        implicit val bS = tp.mS.asInstanceOf[BOOL[s]]
        implicit val iI = tp.mI.asInstanceOf[INT[i]]
        implicit val iF = tp.mF.asInstanceOf[INT[f]]
        num[T].toFixPt[s,i,f](x).asInstanceOf[R]
      case tp: FltPtType[g,e]   =>
        implicit val iG = tp.mG.asInstanceOf[INT[g]]
        implicit val iE = tp.mE.asInstanceOf[INT[e]]
        num[T].toFltPt[g,e](x).asInstanceOf[R]
      case _ => throw new Exception(u"Cannot find way to convert type ")
    }
  }
}
trait NumExp {
  def num[T:Num]: Num[T] = implicitly[Num[T]]
}

trait NumApi extends LowPriorityNumImplicits { this: NumExp =>
  implicit def num2fltpt[T:Num,G:INT,E:INT] = new Cast[T,FltPt[G,E]] {
    @internal def apply(x: T): FltPt[G,E] = num[T].toFltPt[G,E](x)
  }
  implicit def num2fixpt[T:Num,S:BOOL,I:INT,F:INT] = new Cast[T,FixPt[S,I,F]] {
    @internal def apply(x: T): FixPt[S,I,F] = num[T].toFixPt[S,I,F](x)
  }

  implicit def int2numT[T:Type:Num] = new Cast[Int,T] {
    @internal def apply(x: Int): T = num[T].fromInt(x)
  }
  implicit def long2numT[T:Type:Num] = new Cast[Long,T] {
    @internal def apply(x: Long): T = num[T].fromLong(x)
  }
  implicit def float2numT[T:Type:Num] = new Cast[Float,T] {
    @internal def apply(x: Float): T = num[T].fromFloat(x)
  }
  implicit def double2numT[T:Type:Num] = new Cast[Double,T] {
    @internal def apply(x: Double): T = num[T].fromDouble(x)
  }
}
