package argon.lang

import typeclasses._

import argon.core.compiler._
import argon.nodes._
import argon.util.escapeConst
import forge._

/** All Fixed Point Types **/
case class FixPt[S:BOOL,I:INT,F:INT](s: Exp[FixPt[S,I,F]]) extends MetaAny[FixPt[S,I,F]] {
  override type Internal = BigDecimal // TODO: Should use custom type here

  protected val fix = FixPt
  @api def unary_-(): FixPt[S,I,F] = FixPt(fix.neg(this.s))
  @api def unary_~(): FixPt[S,I,F] = FixPt(fix.inv(this.s))
  @api def + (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.add(this.s,that.s))
  @api def - (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.sub(this.s,that.s))
  @api def * (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul(this.s,that.s))
  @api def / (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div(this.s,that.s))
  @api def & (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.and(this.s,that.s))
  @api def | (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.or(this.s,that.s))
  @api def ^ (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.xor(this.s,that.s))
  @api def < (that: FixPt[S,I,F]): MBoolean     = Boolean( fix.lt(this.s,that.s))
  @api def <=(that: FixPt[S,I,F]): MBoolean     = Boolean(fix.leq(this.s,that.s))
  @api def > (that: FixPt[S,I,F]): MBoolean     = Boolean( fix.lt(that.s,this.s))
  @api def >=(that: FixPt[S,I,F]): MBoolean     = Boolean(fix.leq(that.s,this.s))

  // Unbiased rounding operators
  @api def *& (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_unbias(this.s,that.s))
  @api def /& (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_unbias(this.s,that.s))

  // Saturating operators
  @api def <+> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.add_sat(this.s,that.s))
  @api def <-> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.sub_sat(this.s,that.s))
  @api def <*> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_sat(this.s,that.s))
  @api def </> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_sat(this.s,that.s))

  // Saturating and unbiased rounding operators
  @api def <*&> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_unb_sat(this.s,that.s))
  @api def </&> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_unb_sat(this.s,that.s))

  @api def <<(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.lsh(this.s, that.s))  // Left shift
  @api def >>(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.rsh(this.s, that.s))  // Right shift (signed)
  @api def >>>(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.ursh(this.s, that.s)) // Right shift (unsigned)

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

  @internal def intParam(c: Int): Param[Int32] = parameter(IntType)(FixPt.literalToBigDecimal[TRUE,_32,_0](c, force=true))
  @internal def int32(x: BigDecimal): Const[Int32] = const[TRUE,_32,_0](x, force = false)
  @internal def int64(x: BigDecimal): Const[Int64] = const[TRUE,_64,_0](x, force = false)

  implicit class FixPtIntLikeOps[S:BOOL,I:INT](x: FixPt[S,I,_0]) {
    @api def %(y: FixPt[S,I,_0]): FixPt[S,I,_0] = FixPt(FixPt.mod(x.s, y.s))
  }

  /** Type classes **/
  implicit def fixPtIsStaged[S:BOOL,I:INT,F:INT]: Type[FixPt[S,I,F]] = FixPtType[S,I,F](BOOL[S],INT[I],INT[F])
  implicit def fixPtIsNum[S:BOOL,I:INT,F:INT]: Num[FixPt[S,I,F]] = FixPtNum[S,I,F]

  @api implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int): FixPt[S,I,F] = FixPt.lift[S,I,F](x, force=false)
  @api implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long): FixPt[S,I,F] = FixPt.lift[S,I,F](x, force=false)

  /** Rewrite rules **/
  /*@rewrite def MBoolean$not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Def(node@FixNeq(a,b)) => stage( FixEql(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixEql(a,b)) => stage( FixNeq(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def( node@FixLt(a,b)) => stage( FixLeq(b,a)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixLeq(a,b)) => stage(  FixLt(b,a)(node.mS,node.mI,node.mF) )(ctx)
  }*/


  /** Constants **/
  @internal def literalToBigDecimal[S:BOOL,I:INT,F:INT](x: Any, force: CBoolean): BigDecimal = {
    log(c"Creating fixed point constant for $x")

    val sign = BOOL[S].v
    val ibits = INT[I].v
    val fbits = INT[F].v

    val tp = FixPtType[S,I,F]

    val MAX_INTEGRAL_VALUE = BigDecimal( if (sign) (BigInt(1) << (ibits-1)) - 1 else (BigInt(1) << ibits) - 1 )
    val MIN_INTEGRAL_VALUE = BigDecimal( if (sign) -(BigInt(1) << (ibits-1)) else BigInt(0) )

    def makeFixPt(v: BigDecimal): BigDecimal = {
      if (v > MAX_INTEGRAL_VALUE) {
        if (!force) {
          error(ctx, u"Loss of precision detected in implicit lift: $tp cannot represent value ${escapeConst(v)}.")
          error(u"""Use the explicit annotation "${escapeConst(v)}.to[$tp]" to ignore this error.""")
          error(ctx)
        }
        MAX_INTEGRAL_VALUE
      }
      else if (v < MIN_INTEGRAL_VALUE) {
        if (!force) {
          error(ctx, u"Loss of precision detected in implicit lift: $tp cannot represent value ${escapeConst(v)}.")
          error(u"""Use the explicit annotation "${escapeConst(v)}.to[$tp]" to ignore this error.""")
          error(ctx)
        }
        MIN_INTEGRAL_VALUE
      }
      else v
    }

    x match {
      case x: BigDecimal => makeFixPt(x)
      case x: BigInt => makeFixPt(BigDecimal(x))
      case x: Int => makeFixPt(BigDecimal(x))
      case x: Long => makeFixPt(BigDecimal(x))
      case x: Float => makeFixPt(BigDecimal(x.toDouble))
      case x: Double => makeFixPt(BigDecimal(x))
      case x: CString if !x.exists(_ == '.') => makeFixPt(BigDecimal(x))
      case c =>
        error(ctx, s"$c cannot be lifted to a fixed point value")
        error(ctx)
        BigDecimal(0)
    }
  }
  @internal def const[S:BOOL,I:INT,F:INT](x: Any, force: CBoolean = true): Const[FixPt[S,I,F]] = {
    constant(FixPtType[S,I,F])(literalToBigDecimal[S,I,F](x,force))
  }

  /** Constructors **/
  @internal def neg[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = x match {
    case Const(c: BigDecimal) => const[S,I,F](-c)
    case Op(FixNeg(x)) => x
    case _ => stage(FixNeg(x))(ctx)
  }
  @internal def inv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = x match {
    case Const(c: BigDecimal) if c.isWhole => const[S,I,F](BigDecimal(~c.toBigInt))
    case Op(FixInv(x)) => x
    case _ => stage(FixInv(x))(ctx)
  }
  @internal def add[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a + b)
    case (a, Const(0)) => a                               // a + 0 => a
    case (Const(0), b) => b                               // 0 + a => a
    case (a, Op(FixNeg(b))) if a == b => const[S,I,F](0)  // a + -a => 0
    case (Op(FixNeg(a)), b) if a == b => const[S,I,F](0)  // -a + a => 0
    case (Op(FixSub(a,b)), c) if b == c => a              // a - b + b => a
    case (a, Op(FixSub(b,c))) if a == c => b              // a + (b - a) => b
    case _ => stage(FixAdd(x,y))(ctx)
  }
  @internal def add_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[S,I,F](a + b)
    case (a, Const(0)) => a                               // a + 0 => a
    case (Const(0), b) => b                               // 0 + a => a
    case (a, Op(FixNeg(b))) if a == b => const[S,I,F](0)  // a + -a => 0
    case (Op(FixNeg(a)), b) if a == b => const[S,I,F](0)  // -a + a => 0
    case (Op(FixSub(a,b)), c) if b == c => a              // a - b + b => a
    case (a, Op(FixSub(b,c))) if a == c => b              // a + (b - a) => b
    case _ => stage(SatAdd(x,y))(ctx)
  }
  @internal def sub[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[S,I,F](a - b)
    case (a, Const(0)) => a                                      // a - 0 => a
    case (Const(0), a) => stage(FixNeg(a))(ctx)                  // 0 - a => -a
    case (Op(FixAdd(a,b)), c) if a == c => b                     // a + b - a => b
    case (a, Op(FixAdd(b,c))) if a == c => stage(FixNeg(b))(ctx) // a - (b + a) => -b
    case (a, Op(FixAdd(b,c))) if a == b => stage(FixNeg(c))(ctx) // a - (a + b) => -b
    case _ => stage(FixSub(x,y))(ctx)
  }
  @internal def sub_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[S,I,F](a - b)
    case (a, Const(0)) => a                                      // a - 0 => a
    case (Const(0), a) => stage(FixNeg(a))(ctx)                  // 0 - a => -a
    case (Op(FixAdd(a,b)), c) if a == c => b                     // a + b - a => b
    case (a, Op(FixAdd(b,c))) if a == c => stage(FixNeg(b))(ctx) // a - (b + a) => -b
    case (a, Op(FixAdd(b,c))) if a == b => stage(FixNeg(c))(ctx) // a - (a + b) => -b
    case _ => stage(SatSub(x,y))(ctx)
  }

  @internal def mul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a * b)
    case (_, b@Const(0)) => b
    case (a@Const(0), _) => a
    case (a, Const(1)) => a
    case (Const(1), b) => b
    case _ => stage(FixMul(x, y) )(ctx)
  }
  @internal def mul_unb_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a * b)
    case (_, b@Const(0)) => b
    case (a@Const(0), _) => a
    case (a, Const(1)) => a
    case (Const(1), b) => b
    case _ => stage(UnbSatMul(x, y) )(ctx)
  }
  @internal def mul_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a * b)
    case (_, b@Const(0)) => b
    case (a@Const(0), _) => a
    case (a, Const(1)) => a
    case (Const(1), b) => b
    case _ => stage(SatMul(x, y) )(ctx)
  }
  @internal def mul_unbias[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a * b)
    case (_, b@Const(0)) => b
    case (a@Const(0), _) => a
    case (a, Const(1)) => a
    case (Const(1), b) => b
    case _ => stage(UnbMul(x, y) )(ctx)
  }
  @internal def div[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a / b)
    case (a, Const(1)) => a
    case (_, Const(0)) => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(FixDiv(x,y))(ctx)
  }
  @internal def div_unb_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a / b)
    case (a, Const(1)) => a
    case (_, Const(0)) => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(UnbSatDiv(x,y))(ctx)
  }
  @internal def div_sat[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a / b)
    case (a, Const(1)) => a
    case (_, Const(0)) => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(SatDiv(x,y))(ctx)
  }
  @internal def div_unbias[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,F](a / b)
    case (a, Const(1)) => a
    case (_, Const(0)) => warn(ctx, "Division by constant 0 detected"); stage(UnbDiv(x,y))(ctx)
    case _ => stage(UnbDiv(x,y))(ctx)
  }
  @internal def and[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isWhole => const[S,I,F](BigDecimal(a.toBigInt & b.toBigInt))
    case (a@Const(0), _) => a
    case (_, b@Const(0)) => b
    case _ => stage(FixAnd(x,y))(ctx)
  }
  @internal def or[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isWhole => const[S,I,F](BigDecimal(a.toBigInt | b.toBigInt))
    case (a, Const(0)) => a
    case (Const(0), b) => b
    case _ => stage(FixOr(x,y))(ctx)
  }
  @internal def xor[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isWhole => const[S,I,F](BigDecimal(a.toBigInt ^ b.toBigInt))
    case (a, Const(0)) => a
    case (Const(0), b) => b
    case _ => stage(FixXor(x,y))(ctx)
  }
  @internal def lt[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Boolean.const(a < b)
    case _ => stage( FixLt(x,y))(ctx)
  }

  @internal def leq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Boolean.const(a <= b)
    case _ => stage(FixLeq(x,y))(ctx)
  }
  @internal def neq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Boolean.const(a != b)
    case _ => stage(FixNeq(x,y))(ctx)
  }
  @internal def eql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[MBoolean] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Boolean.const(a == b)
    case _ => stage(FixEql(x,y))(ctx)
  }
  @internal def mod[S:BOOL,I:INT](x: Exp[FixPt[S,I,_0]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,_0]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => const[S,I,_0](a % b)
    case (a, Const(1)) => const[S,I,_0](0)
    case _ => stage(FixMod(x,y))(ctx)
  }

  @internal def lsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isValidInt => const[S,I,F](BigDecimal(a.toBigInt << b.toInt))
    case (a, Const(0)) => a
    case _ => stage(FixLsh(x,y))(ctx)
  }
  @internal def rsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isValidInt => const[S,I,F](BigDecimal(a.toBigInt >> b.toInt))
    case (a, Const(0)) => a
    case _ => stage(FixRsh(x,y))(ctx)
  }
  @internal def ursh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isValidLong && b.isValidInt => const[S,I,F](BigDecimal(a.toLong >>> b.toInt))
    case (a, Const(0)) => a
    case _ => stage(FixURsh(x,y))(ctx)
  }

  @internal def random[S:BOOL,I:INT,F:INT](max: Option[Exp[FixPt[S,I,F]]]): Exp[FixPt[S,I,F]] = {
    stageSimple(FixRandom[S,I,F](max))(ctx)
  }

  @internal def convert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Exp[FixPt[_,_,_]]): Exp[FixPt[S2,I2,F2]] = {
    stage(FixConvert[S,I,F,S2,I2,F2](x.asInstanceOf[Exp[FixPt[S,I,F]]]))(ctx)
  }

  @internal def to_flt[S:BOOL,I:INT,F:INT,G:INT,E:INT](x: Exp[FixPt[S,I,F]]): Exp[FltPt[G,E]] = {
    stage(FixPtToFltPt[S,I,F,G,E](x))(ctx)
  }

  @internal def from_text[S:BOOL,I:INT,F:INT](x: Exp[MString]): Exp[FixPt[S,I,F]] = x match {
    case Const(c: CString) =>
      if (c.indexOf("0x") == 0) {
        val raw = c.replace("0x","")
        val digits = raw.length
        val dec = raw.zipWithIndex.map{case (d, i) => scala.math.pow(16, digits-1-i).toInt*d.toInt}.sum
        FixPt[S,I,F](dec).s
      }
      else {
        FixPt[S,I,F](c).s
      }
    case _ => stage(StringToFixPt[S,I,F](x))(ctx)
  }
}

/** Direct methods **/
trait FixPtExp {
  def isFixPtType(x: Type[_]) = FixPtType.unapply(x).isDefined
  def isInt32Type(x: Type[_]) = IntType.unapply(x)
  def isIndexType(x: Type[_]) = x == FixPtType[TRUE,_32,_0]

  @internal def intParam(c: Int): Param[Int32] = FixPt.intParam(c)
  @internal def int32(x: BigDecimal): Const[Int32] = FixPt.int32(x)
  @internal def int64(x: BigDecimal): Const[Int64] = FixPt.int64(x)

  /** Lifting **/
  implicit object LiftInt extends Lift[Int,Int32] {
    @internal def apply(x: Int): Int32 = FixPt.int2fixpt(x)
  }
  implicit object LiftLong extends Lift[Long,Int64] {
    @internal def apply(x: Long): Int64 = FixPt.long2fixpt(x)
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
    @internal def apply(x: MString): FixPt[S,I,F] = wrap(FixPt.from_text[S,I,F](x.s))
  }
}

trait FixPtApi {
  type Long  = FixPt[TRUE,_64,_0]
  type Int   = FixPt[TRUE,_32,_0]
  type Short = FixPt[TRUE,_16,_0]
  type Char  = FixPt[TRUE, _8,_0]
}

