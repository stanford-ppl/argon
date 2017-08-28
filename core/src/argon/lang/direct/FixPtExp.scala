package argon.lang.direct

import argon.core._
import argon.nodes._
import forge._

/** Direct methods **/
trait FixPtExp {
  def isFixPtType(x: Type[_]) = FixPtType.unapply(x).isDefined
  def isInt32Type(x: Type[_]) = IntType.unapply(x)
  def isIndexType(x: Type[_]) = x == FixPtType[TRUE,_32,_0]

  @internal def intParam(c: Int): Param[Int32] = FixPt.intParam(c)
  @internal def int8(x: CString): Int8 = FixPt.int8(x)
  @internal def int8(x: MString): Int8 = FixPt.int8(x)

  @internal def int8(x: BigDecimal): Int8 = FixPt.int8(x)
  @internal def int32(x: BigDecimal): Int32 = FixPt.int32(x)
  @internal def int64(x: BigDecimal): Int64 = FixPt.int64(x)

  @internal def int8s(x: BigDecimal): Const[Int8] = FixPt.int8s(x)
  @internal def int32s(x: BigDecimal): Const[Int32] = FixPt.int32s(x)
  @internal def int64s(x: BigDecimal): Const[Int64] = FixPt.int64s(x)

  @api implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int): FixPt[S,I,F] = FixPt.lift[S,I,F](x, force=false)
  @api implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long): FixPt[S,I,F] = FixPt.lift[S,I,F](x, force=false)

  @api def unif[F:INT]()(implicit ctx: SrcCtx): FixPt[FALSE, _0, F] = FixPt[FALSE, _0, F](FixPt.unif[FALSE, _0, F]())

  /** Lifting **/
  implicit object LiftInt extends Lift[Int,Int32] {
    @internal def apply(x: Int): Int32 = int2fixpt(x)
  }
  implicit object LiftLong extends Lift[Long,Int64] {
    @internal def apply(x: Long): Int64 = long2fixpt(x)
  }

  /** Casting **/
  implicit def int_cast_fixpt[S:BOOL,I:INT,F:INT]: Cast[Int,FixPt[S,I,F]] = new Cast[Int,FixPt[S,I,F]] {
    @internal def apply(x: Int): FixPt[S,I,F] = FixPt.lift[S,I,F](x, force=true)
  }
  implicit def long_cast_fixpt[S:BOOL,I:INT,F:INT]: Cast[Long,FixPt[S,I,F]] = new Cast[Long,FixPt[S,I,F]] {
    @internal def apply(x: Long): FixPt[S,I,F] = FixPt.lift[S,I,F](x, force=true)
  }
  implicit def fixpt2fixpt[S:BOOL,I:INT,F:INT, S2:BOOL,I2:INT,F2:INT] = new Cast[FixPt[S,I,F],FixPt[S2,I2,F2]] {
    @internal def apply(x: FixPt[S,I,F]): FixPt[S2,I2,F2] = wrap(FixPt.convert[S,I,F,S2,I2,F2](x.s))
  }
  implicit def fixpt2fltpt[S:BOOL,I:INT,F:INT, G:INT,E:INT] = new Cast[FixPt[S,I,F],FltPt[G,E]] {
    @internal def apply(x: FixPt[S,I,F]): FltPt[G,E] = wrap(FixPt.to_flt[S,I,F,G,E](x.s))
  }
  implicit def string2fixpt[S:BOOL,I:INT,F:INT] = new Cast[MString,FixPt[S,I,F]] {
    @internal def apply(x: MString): FixPt[S,I,F] = wrap(FixPt.from_string[S,I,F](x.s))
  }
}

