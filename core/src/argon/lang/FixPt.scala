package argon.lang

import typeclasses._

import argon.core._
import argon.nodes._
import argon.util.escapeConst
import forge._
import argon.util._

/** All Fixed Point Types **/
case class FixPt[S:BOOL,I:INT,F:INT](s: Exp[FixPt[S,I,F]]) extends MetaAny[FixPt[S,I,F]] {
  override type Internal = FixedPoint

  private val fix = FixPt
  /** Returns negation of this fixed point value. **/
  @api def unary_-(): FixPt[S,I,F] = FixPt(fix.neg(this.s))
  /** Returns bitwise inversion of this fixed point value. **/
  @api def unary_~(): FixPt[S,I,F] = FixPt(fix.inv(this.s))
  /** Fixed point addition. **/
  @api def + (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.add(this.s,that.s))
  /** Fixed point subtraction. **/
  @api def - (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.sub(this.s,that.s))
  /** Fixed point multiplication. **/
  @api def * (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul(this.s,that.s))
  /** Fixed point division. **/
  @api def / (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div(this.s,that.s))
  /** Fixed point modulus. **/
  @api def % (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mod(this.s,that.s))
  /** Bit-wise AND. **/
  @api def & (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.and(this.s,that.s))
  /** Bit-wise OR. **/
  @api def | (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.or(this.s,that.s))
  /** Bit-wise XOR. **/
  @api def ^ (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.xor(this.s,that.s))
  /** Logical shift left. **/
  @api def <<(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.lsh(this.s, that.s))
  /** Arithmetic (sign-preserving) shift right. **/
  @api def >>(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.rsh(this.s, that.s))
  /** Logical (zero-padded) shift right. **/
  @api def >>>(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.ursh(this.s, that.s))

  /**
    * Less than comparison.
    *
    * Returns `true` if this value is less than `that` value. Otherwise returns `false`.
    */
  @api def < (that: FixPt[S,I,F]): MBoolean     = Boolean( fix.lt(this.s,that.s))
  /**
    * Less than or equal comparison.
    *
    * Returns `true` if this value is less than or equal to `that` value. Otherwise returns `false`.
    */
  @api def <=(that: FixPt[S,I,F]): MBoolean     = Boolean(fix.leq(this.s,that.s))
  /**
    * Greater than comparison
    *
    * Returns `true` if this value is greater than `that` value. Otherwise returns `false`.
    */
  @api def > (that: FixPt[S,I,F]): MBoolean     = Boolean( fix.lt(that.s,this.s))
  /**
    * Greater than or equal comparison.
    *
    * Returns `true` if this value is greater than or equal to `that` value. Otherwise returns `false`.
    */
  @api def >=(that: FixPt[S,I,F]): MBoolean     = Boolean(fix.leq(that.s,this.s))

  // Unbiased rounding operators
  /**
    * Fixed point multiplication with unbiased rounding.
    *
    * After multiplication, probabilistically rounds up or down to the closest representable number.
    */
  @api def *& (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_unbias(this.s,that.s))

  /**
    * Fixed point division with unbiased rounding.
    *
    * After division, probabilistically rounds up or down to the closest representable number.
    */
  @api def /& (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_unbias(this.s,that.s))

  // Saturating operators
  /**
    * Saturating fixed point addition.
    *
    * Addition which saturates at the largest or smallest representable number upon over/underflow.
    */
  @api def <+> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.add_sat(this.s,that.s))
  /**
    * Saturating fixed point subtraction.
    *
    * Subtraction which saturates at the largest or smallest representable number upon over/underflow.
    */
  @api def <-> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.sub_sat(this.s,that.s))
  /**
    * Saturating fixed point multiplication.
    *
    * Multiplication which saturates at the largest or smallest representable number upon over/underflow.
    */
  @api def <*> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_sat(this.s,that.s))
  /**
    * Saturating fixed point division.
    *
    * Division which saturates at the largest or smallest representable number upon over/underflow.
    */
  @api def </> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_sat(this.s,that.s))

  // Saturating and unbiased rounding operators
  /**
    * Saturating fixed point multiplication with unbiased rounding.
    *
    * After multiplication, probabilistically rounds up or down to the closest representable number.
    * After rounding, also saturates at the largest or smallest representable number upon over/underflow.
    */
  @api def <*&> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_unb_sat(this.s,that.s))
  /**
    * Saturating fixed point division with unbiased rounding.
    *
    * After division, probabilistically rounds up or down to the closest representable number.
    * After rounding, also saturates at the largest or smallest representable number upon over/underflow.
    */
  @api def </&> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_unb_sat(this.s,that.s))

  @api def ===(that: FixPt[S,I,F]) = Boolean(fix.eql(this.s, that.s))
  @api def =!=(that: FixPt[S,I,F]) = Boolean(fix.neq(this.s, that.s))
  @api override def toText = String.ify(this)
}

object FixPt {
  /** Static methods **/
  @internal def wrapped[S:BOOL,I:INT,F:INT](s: Exp[FixPt[S,I,F]]) = new FixPt[S,I,F](s)
  @internal def lift[S:BOOL,I:INT,F:INT](s: Any, force: CBoolean) = FixPt.wrapped(const[S,I,F](s, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: Int): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))
  @api def apply[S:BOOL,I:INT,F:INT](x: Long): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))
  @api def apply[S:BOOL,I:INT,F:INT](x: Float): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))
  @api def apply[S:BOOL,I:INT,F:INT](x: Double): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))
  @api def apply[S:BOOL,I:INT,F:INT](x: CString): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))
  @api def apply[S:BOOL,I:INT,F:INT](x: BigInt): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))
  @api def apply[S:BOOL,I:INT,F:INT](x: BigDecimal): FixPt[S,I,F] = FixPt.wrapped(const[S,I,F](x))

  @internal def intParam(c: Int): Param[Index] = parameter(IntType)(literalToFixedPoint[TRUE,_32,_0](c, force=true))
  @internal def int8(x: BigDecimal): Int8 = wrap(const[TRUE,_8,_0](x, force = false))
  @internal def int8(x: MString): Int8 = wrap(char_2_int(x.s))
  @internal def int8(x: CString): Int8 = int8(String(x))
  @internal def int16(x: BigDecimal): Int16 = wrap(const[TRUE,_16,_0](x, force = false))
  @internal def int32(x: BigDecimal): Index = wrap(const[TRUE,_32,_0](x, force = false))
  @internal def int64(x: BigDecimal): Int64 = wrap(const[TRUE,_64,_0](x, force = false))

  @internal def int8s(x: Int): Const[Int8] = const[TRUE,_8,_0](x, force = false)
  @internal def int32s(x: Int): Const[Index] = const[TRUE,_32,_0](x, force = false)
  @internal def int64s(x: Int): Const[Int64] = const[TRUE,_64,_0](x, force = false)

  @internal def int8s(x: BigDecimal): Const[Int8] = const[TRUE,_8,_0](x, force = false)
  @internal def int32s(x: BigDecimal): Const[Index] = const[TRUE,_32,_0](x, force = false)
  @internal def int64s(x: BigDecimal): Const[Int64] = const[TRUE,_64,_0](x, force = false)

  /** Type classes **/
  implicit def fixPtIsStaged[S:BOOL,I:INT,F:INT]: Type[FixPt[S,I,F]] = FixPtType[S,I,F](BOOL[S],INT[I],INT[F])
  implicit def fixPtIsNum[S:BOOL,I:INT,F:INT]: Num[FixPt[S,I,F]] = FixPtNum[S,I,F]

  /** Rewrite rules **/
  /*@rewrite def MBoolean$not(x: Exp[MBoolean])(implicit ctx: SrcCtx): Exp[MBoolean] = x match {
    case Def(node@FixNeq(a,b)) => stage( FixEql(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixEql(a,b)) => stage( FixNeq(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def( node@FixLt(a,b)) => stage( FixLeq(b,a)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixLeq(a,b)) => stage(  FixLt(b,a)(node.mS,node.mI,node.mF) )(ctx)
  }*/


  /** Constants **/
  @internal private def literalToFixedPoint[S:BOOL,I:INT,F:INT](x: Any, force: CBoolean): FixedPoint = {
    log(c"Creating fixed point constant for $x")

    val sign = BOOL[S].v
    val ibits = INT[I].v
    val fbits = INT[F].v
    val fmt = FixFormat(sign,ibits,fbits)
    val tp  = FixPtType[S,I,F]

    def withCheck[T](x: T)(eql: T => CBoolean): T = {
      if (!force && !eql(x)) {
        error(ctx, u"Loss of precision detected in implicit lift: $tp cannot fully represent value ${escapeConst(x)}.")
        error(u"""Use the explicit annotation "${escapeConst(x)}.to[$tp]" to ignore this error.""")
        error(ctx)
      }
      x
    }

    x match {
      case x: BigDecimal => withCheck(FixedPoint(x, fmt)){ _.toBigDecimal == x }
      case x: BigInt     => withCheck(FixedPoint(x, fmt)){ _.toBigInt == x }
      case x: Int        => withCheck(FixedPoint(x, fmt)){ _.toInt == x }
      case x: Long       => withCheck(FixedPoint(x, fmt)){ _.toLong == x }
      case x: Float      => withCheck(FixedPoint(x, fmt)){ _.toFloat == x }
      case x: Double     => withCheck(FixedPoint(x, fmt)){ _.toDouble == x }
      case x: CString    => withCheck(FixedPoint(x, fmt)){ _.toBigDecimal == BigDecimal(x) }
      case x: FixedPoint if x.fmt == fmt => x
      case x: FixedPoint => withCheck(x.toFixedPoint(fmt)){ _.toFixedPoint(x.fmt) == x }
      case c =>
        error(ctx, s"$c cannot be lifted to a fixed point value")
        error(ctx)
        FixedPoint(0, fmt)
    }
  }
  @internal def const[S:BOOL,I:INT,F:INT](x: Any, force: CBoolean = true): Const[FixPt[S,I,F]] = {
    constant(FixPtType[S,I,F])(literalToFixedPoint[S,I,F](x,force))
  }

  /** Constructors **/
  @internal def char_2_int(x: Exp[MString]): Exp[Int8] = stage(Char2Int(x))(ctx)
  @internal def neg[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = x match {
    case Literal(c) => const[S,I,F](-c)
    //case Const(c: FixedPoint) => const[S,I,F](-c)
    case Op(FixNeg(x)) => x
    case _ => stage(FixNeg(x))(ctx)
  }
  @internal def inv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = x match {
    case Literal(c)    => const[S,I,F](~c)
    case Op(FixInv(x)) => x
    case _ => stage(FixInv(x))(ctx)
  }
  @internal def add[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))       => const[S,I,F](a + b)
    case (a, Literal(b)) if b == 0      => a                   // a + 0 => a
    case (Literal(a), b) if a == 0      => b                   // 0 + a => a
    case (a, Op(FixNeg(b))) if a == b   => const[S,I,F](0)     // a + -a => 0
    case (Op(FixNeg(a)), b) if a == b   => const[S,I,F](0)     // -a + a => 0
    case (Op(FixSub(a,b)), c) if b == c => a                   // a - b + b => a
    case (a, Op(FixSub(b,c))) if a == c => b                   // a + (b - a) => b
    case _ => stage(FixAdd(x,y))(ctx)
  }
  @internal def add_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b)) => const[S,I,F](a <+> b)
    case (a, Literal(b)) if b == 0      => a                   // a + 0 => a
    case (Literal(a), b) if a == 0      => b                   // 0 + a => a
    case (a, Op(FixNeg(b))) if a == b   => const[S,I,F](0)     // a + -a => 0
    case (Op(FixNeg(a)), b) if a == b   => const[S,I,F](0)     // -a + a => 0
    case (Op(FixSub(a,b)), c) if b == c => a                   // a - b + b => a
    case (a, Op(FixSub(b,c))) if a == c => b                   // a + (b - a) => b
    case _ => stage(SatAdd(x,y))(ctx)
  }
  @internal def sub[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))       => const[S,I,F](a - b)
    case (a, Literal(b)) if b == 0      => a                          // a - 0 => a
    case (Literal(a), b) if a == 0      => stage(FixNeg(b))(ctx)      // 0 - a => -a
    case (Op(FixAdd(a,b)), c) if a == c => b                          // a + b - a => b
    case (a, Op(FixAdd(b,c))) if a == c => stage(FixNeg(b))(ctx)      // a - (b + a) => -b
    case (a, Op(FixAdd(b,c))) if a == b => stage(FixNeg(c))(ctx)      // a - (a + b) => -b
    case _ => stage(FixSub(x,y))(ctx)
  }
  @internal def sub_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))       => const[S,I,F](a <-> b)
    case (a, Literal(b)) if b == 0      => a                          // a - 0 => a
    case (Literal(a), b) if a == 0      => stage(FixNeg(b))(ctx)      // 0 - a => -a
    case (Op(FixAdd(a,b)), c) if a == c => b                          // a + b - a => b
    case (a, Op(FixAdd(b,c))) if a == c => stage(FixNeg(b))(ctx)      // a - (b + a) => -b
    case (a, Op(FixAdd(b,c))) if a == b => stage(FixNeg(c))(ctx)      // a - (a + b) => -b
    case _ => stage(SatSub(x,y))(ctx)
  }

  @internal def mod[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a % b)
    case (_, Literal(b)) if b == 1 => const[S,I,F](0)
    case _ => stage(FixMod(x,y))(ctx)
  }

  @internal def mul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b)) => const[S,I,F](a * b)
    case (_, Literal(b)) if b == 0 => const[S,I,F](0)
    case (Literal(a), _) if a == 0 => const[S,I,F](0)
    case (a, Literal(b)) if b == 1 => a
    case (Literal(a), b) if a == 1 => b
    case (_, Literal(b)) if isPow2(b) && b > 0 => lsh(x,const[S,I,_0](log2(b.toDouble).toInt))
    case (_, Literal(b)) if isPow2(b) && b < 0 => lsh(FixPt.neg(x), const[S,I,_0](log2(b.abs.toDouble).toInt))
    case _ => stage(FixMul(x, y) )(ctx)
  }
  @internal def mul_unb_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b)) => const[S,I,F](a <*&> b)
    case (_, Literal(b)) if b == 0 => const[S,I,F](0)
    case (Literal(a), _) if a == 0 => const[S,I,F](0)
    case (a, Literal(b)) if b == 1 => a
    case (Literal(a), b) if a == 1 => b
    case _ => stage(UnbSatMul(x, y) )(ctx)
  }
  @internal def mul_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b)) => const[S,I,F](a <*> b)
    case (_, Literal(b)) if b == 0 => const[S,I,F](0)
    case (Literal(a), _) if a == 0 => const[S,I,F](0)
    case (a, Literal(b)) if b == 1 => a
    case (Literal(a), b) if a == 1 => b
    case _ => stage(SatMul(x, y) )(ctx)
  }
  @internal def mul_unbias[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b)) => const[S,I,F](a *& b)
    case (_, Literal(b)) if b == 0 => const[S,I,F](0)
    case (Literal(a), _) if a == 0 => const[S,I,F](0)
    case (a, Literal(b)) if b == 1 => a
    case (Literal(a), b) if a == 1 => b
    case _ => stage(UnbMul(x, y) )(ctx)
  }
  @internal def div[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b)) => const[S,I,F](a / b)
    case (a, Literal(b)) if b == 1 => a
    case (_, Literal(b)) if b == 0 => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)

    case (_, Literal(b)) if isPow2(b) && b > 0 => rsh(x,const[S,I,_0](log2(b.toDouble).toInt))
    case (_, Literal(b)) if isPow2(b) && b < 0 => rsh(FixPt.neg(x),const[S,I,_0](log2(b.abs.toDouble).toInt))

    case _ => stage(FixDiv(x,y))(ctx)
  }
  @internal def div_unb_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a </&> b)
    case (a, Literal(b)) if b == 1 => a
    case (_, Literal(b)) if b == 0 => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(UnbSatDiv(x,y))(ctx)
  }
  @internal def div_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a </> b)
    case (a, Literal(b)) if b == 1 => a
    case (_, Literal(b)) if b == 0 => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(SatDiv(x,y))(ctx)
  }
  @internal def div_unbias[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a /& b)
    case (a, Literal(b)) if b == 1 => a
    case (_, Literal(b)) if b == 0 => warn(ctx, "Division by constant 0 detected"); stage(UnbDiv(x,y))(ctx)
    case _ => stage(UnbDiv(x,y))(ctx)
  }
  @internal def and[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a & b)
    case (Literal(a), _) if a == 0 => const[S,I,F](0)
    case (_, Literal(b)) if b == 0 => const[S,I,F](0)
    case _ => stage(FixAnd(x,y))(ctx)
  }
  @internal def or[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a | b)
    case (a, Literal(b)) if b == 0 => a
    case (Literal(a), b) if a == 0 => b
    case _ => stage(FixOr(x,y))(ctx)
  }
  @internal def xor[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a ^ b)
    case (a, Literal(b)) if b == 0 => a
    case (Literal(a), b) if a == 0 => b
    case _ => stage(FixXor(x,y))(ctx)
  }
  @internal def lt[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Literal(a), Literal(b)) => Boolean.const(a < b)
    case _ => stage( FixLt(x,y))(ctx)
  }

  @internal def leq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Literal(a), Literal(b)) => Boolean.const(a <= b)
    case _ => stage(FixLeq(x,y))(ctx)
  }
  @internal def neq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Literal(a), Literal(b)) => Boolean.const(a != b)
    case _ => stage(FixNeq(x,y))(ctx)
  }
  @internal def eql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Literal(a), Literal(b)) => Boolean.const(a == b)
    case _ => stage(FixEql(x,y))(ctx)
  }


  @internal def lsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a << b)
    case (a, Literal(b)) if b == 0 => a
    case _ => stage(FixLsh(x,y))(ctx)
  }
  @internal def rsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a >> b)
    case (a, Literal(b)) if b == 0 => a
    case _ => stage(FixRsh(x,y))(ctx)
  }
  @internal def ursh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Literal(a), Literal(b))  => const[S,I,F](a >>> b)
    case (a, Literal(b)) if b == 0 => a
    case _ => stage(FixURsh(x,y))(ctx)
  }

  @internal def random[S:BOOL,I:INT,F:INT](max: Option[Exp[FixPt[S,I,F]]]): Exp[FixPt[S,I,F]] = {
    stageSimple(FixRandom[S,I,F](max))(ctx)
  }

  @internal def unif[S:BOOL,I:INT,F:INT](): Exp[FixPt[S,I,F]] = {
    stageSimple(FixUnif[S,I,F]())(ctx)
  }

  @internal def convert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Exp[FixPt[_,_,_]]): Exp[FixPt[S2,I2,F2]] = {
    // TODO: This is right, but it breaks Matt's bitwidth hacks in codegen
    /*if (BOOL[S] == BOOL[S2] && INT[I] == INT[I2] && INT[F] == INT[F2]) {
      x.asInstanceOf[Exp[FixPt[S2,I2,F2]]]
    }
    else {*/
      stage(FixConvert[S,I,F,S2,I2,F2](x.asInstanceOf[Exp[FixPt[S,I,F]]]))(ctx)
    //}
  }

  @internal def to_flt[S:BOOL,I:INT,F:INT,G:INT,E:INT](x: Exp[FixPt[S,I,F]]): Exp[FltPt[G,E]] = {
    stage(FixPtToFltPt[S,I,F,G,E](x))(ctx)
  }

  @internal def from_string[S:BOOL,I:INT,F:INT](x: Exp[MString]): Exp[FixPt[S,I,F]] = x match {
    case Const(c: CString) =>
      if (c.indexOf("0x") == 0) {
        val raw = c.replace("0x","")
        val digits = raw.length
        val dec = raw.zipWithIndex.map{case (d, i) => scala.math.pow(16, digits-1-i).toInt*d.toInt}.sum
        int2fixpt[S,I,F](dec).s
      }
      else {
        FixPt[S,I,F](c).s
      }
    case _ => stage(StringToFixPt[S,I,F](x))(ctx)
  }
}
