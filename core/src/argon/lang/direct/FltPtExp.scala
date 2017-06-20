package argon.lang.direct

import argon.core._
import argon.nodes._
import forge._

trait FltPtExp {
  /** Direct methods **/
  def isFltPtType(x: Type[_]) = FltPtType.unapply(x).isDefined

  @api implicit def int2fltpt[G:INT,E:INT](x: Int): FltPt[G,E] = FltPt.lift[G,E](x, force=false)
  @api implicit def long2fltpt[G:INT,E:INT](x: Long): FltPt[G,E] = FltPt.lift[G,E](x, force=false)
  @api implicit def float2fltpt[G:INT,E:INT](x: Float): FltPt[G,E] = FltPt.lift[G,E](x, force=false)
  @api implicit def double2fltpt[G:INT,E:INT](x: Double): FltPt[G,E] = FltPt.lift[G,E](x, force=false)

  /** Lifting **/
  implicit object Float2FltPt extends Lift[Float,Float32] {
    @internal def apply(x: Float): Float32 = float2fltpt(x)
  }
  implicit object Double2FltPt extends Lift[Double,Float64] {
    @internal def apply(x: Double): Float64 = double2fltpt(x)
  }

  /** Casting **/
  implicit def int_cast_fltpt[G:INT,E:INT]: Cast[Int,FltPt[G,E]] = new Cast[Int,FltPt[G,E]] {
    @internal def apply(x: Int): FltPt[G,E] = FltPt.lift[G,E](x, force=true)
  }
  implicit def long_cast_fltpt[G:INT,E:INT]: Cast[Long,FltPt[G,E]] = new Cast[Long,FltPt[G,E]] {
    @internal def apply(x: Long): FltPt[G,E] = FltPt.lift[G,E](x, force=true)
  }
  implicit def float_cast_fltpt[G:INT,E:INT]: Cast[Float,FltPt[G,E]] = new Cast[Float,FltPt[G,E]] {
    @internal def apply(x: Float): FltPt[G,E] = FltPt.lift[G,E](x, force=true)
  }
  implicit def double_cast_fltpt[G:INT,E:INT]: Cast[Double,FltPt[G,E]] = new Cast[Double,FltPt[G,E]] {
    @internal def apply(x: Double): FltPt[G,E] = FltPt.lift[G,E](x, force=true)
  }

  implicit def fltpt2fltpt[G:INT,E:INT, G2:INT,E2:INT] = new Cast[FltPt[G,E],FltPt[G2,E2]] {
    @internal def apply(x: FltPt[G,E]): FltPt[G2,E2] = FltPt(FltPt.convert[G,E,G2,E2](x.s))
  }
  implicit def fltpt2fixpt[G:INT,E:INT,S:BOOL,I:INT,F:INT] = new Cast[FltPt[G,E],FixPt[S,I,F]] {
    @internal def apply(x: FltPt[G,E]): FixPt[S,I,F] = FixPt(FltPt.to_fix[G,E,S,I,F](x.s))
  }
  implicit def string2fltpt[G:INT,E:INT] = new Cast[MString,FltPt[G,E]] {
    @internal def apply(x: MString): FltPt[G,E] = FltPt(FltPt.from_string[G,E](x.s))
  }
}
