package argon.lang

import argon._
import argon.core.UserFacing
import argon.typeclasses._
import argon.utils.escapeConst
import forge._

/** Infix Methods **/
case class FixPt[S:BOOL,I:INT,F:INT](s: Exp[FixPt[S,I,F]]) extends MetaAny[FixPt[S,I,F]] {
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
  @api def < (that: FixPt[S,I,F]): Bool         = Bool( fix.lt(this.s,that.s))
  @api def <=(that: FixPt[S,I,F]): Bool         = Bool(fix.leq(this.s,that.s))
  @api def > (that: FixPt[S,I,F]): Bool         = Bool( fix.lt(that.s,this.s))
  @api def >=(that: FixPt[S,I,F]): Bool         = Bool(fix.leq(that.s,this.s))

  // Unbiased rounding operators
  @api def *& (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_unbias(this.s,that.s))
  @api def /& (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_unbias(this.s,that.s))

  // Saturating operatiors
  @api def <+> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.add_sat(this.s,that.s))
  @api def <-> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.sub_sat(this.s,that.s))
  @api def <*> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_sat(this.s,that.s))
  @api def </> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_sat(this.s,that.s))

  // Saturating and unbiased rounding operatiors
  @api def <*&> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.mul_unb_sat(this.s,that.s))
  @api def </&> (that: FixPt[S,I,F]): FixPt[S,I,F] = FixPt(fix.div_unb_sat(this.s,that.s))

  @api def <<(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.lsh(this.s, that.s))  // Left shift
  @api def >>(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.rsh(this.s, that.s))  // Right shift (signed)
  @api def >>>(that: FixPt[S,I,_0]): FixPt[S,I,F] = FixPt(fix.ursh(this.s, that.s)) // Right shift (unsigned)

  @api def ===(that: FixPt[S,I,F]) = Bool(fix.eql(this.s, that.s))
  @api def =!=(that: FixPt[S,I,F]) = Bool(fix.neq(this.s, that.s))
  @api override def toText = Text.ify(this)
}

sealed class FixPtType[S,I,F](val mS: BOOL[S], val mI: INT[I], val mF: INT[F]) extends Type[FixPt[S,I,F]] with CanBits[FixPt[S,I,F]] {
  def wrapped(s: Exp[FixPt[S,I,F]]): FixPt[S,I,F] = FixPt[S,I,F](s)(mS,mI,mF)
  def stagedClass = classOf[FixPt[S,I,F]]
  def isPrimitive = true

  def isSigned: Boolean = mS.v
  def intBits: Int = mI.v
  def fracBits: Int = mF.v

  override def equals(x: Any) = x match {
    case that: FixPtType[_,_,_] => this.mS == that.mS && this.mI == that.mI && this.mF == that.mF
    case _ => false
  }
  override def hashCode() = (mS,mI,mF).##
  protected def getBits(children: Seq[Type[_]]) = Some(FixPtNum[S,I,F](mS,mI,mF))

  override def toString = s"FixPt[$mS,$mI,$mF]"
}

class FixPtNum[S:BOOL,I:INT,F:INT] extends Num[FixPt[S,I,F]] {
  @api def negate(x: FixPt[S,I,F]) = -x
  @api def plus(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x + y
  @api def minus(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x - y
  @api def times(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x * y
  @api def divide(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x / y

  @api def zero = FixPt[S,I,F](0, force = true)
  @api def one = FixPt[S,I,F](1, force = true)
  @api def random(max: Option[FixPt[S,I,F]]): FixPt[S, I, F] = FixPt[S, I, F](FixPt.random[S, I, F](max.map(_.s)))
  @api def length: Int = INT[I].v + INT[F].v

  @api def lessThan(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x < y
  @api def lessThanOrEqual(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x <= y
  @api def equal(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x === y

  @api def toFixPt[S2:BOOL,I2:INT,F2:INT](x: FixPt[S,I,F]): FixPt[S2,I2,F2] = FixPt(FixPt.convert[S,I,F,S2,I2,F2](x.s))
  @api def toFltPt[G:INT,E:INT](x: FixPt[S,I,F]): FltPt[G,E] = FltPt(FixPt.to_flt[S,I,F,G,E](x.s))

  @api def fromInt(x: Int, force: Boolean = true) = FixPt[S,I,F](x, force)
  @api def fromLong(x: Long, force: Boolean = true) = FixPt[S,I,F](x, force)
  @api def fromFloat(x: Float, force: Boolean = true) = FixPt[S,I,F](x, force)
  @api def fromDouble(x: Double, force: Boolean = true) = FixPt[S,I,F](x, force)
}
object FixPtNum {
  def apply[S:BOOL,I:INT,F:INT] = new FixPtNum[S,I,F]
}

object FixPtType {
  def apply[S:BOOL,I:INT,F:INT] = new FixPtType[S,I,F](BOOL[S],INT[I],INT[F])

  def unapply(x:Type[_]):Option[(Boolean, Int, Int)] = x match {
    case tp:FixPtType[_,_,_] => Some((tp.isSigned, tp.intBits, tp.fracBits))
    case _ => None
  }
}

object IntType extends FixPtType(BOOL[TRUE],INT[_32],INT[_0]) with UserFacing {
  def unapply(x: Type[_]): Boolean = x match {
    case FixPtType(true, 32, 0) => true
    case _ => false
  }
  override def toStringUser = "Int"
}

object LongType extends FixPtType(BOOL[TRUE],INT[_64],INT[_0]) with UserFacing {
  def unapply(x: Type[_]): Boolean = x match {
    case FixPtType(true, 64, 0) => true
    case _ => false
  }
  override def toStringUser = "Long"
}

trait FixPtExp {
  implicit class FixPtIntLikeOps[S:BOOL,I:INT](x: FixPt[S,I,_0]) {
    @api def %(y: FixPt[S,I,_0]): FixPt[S,I,_0] = FixPt(FixPt.mod(x.s, y.s))
  }
}

object FixPt {
  /** Static methods **/
  @internal def apply[S:BOOL,I:INT,F:INT](s: Exp[FixPt[S,I,F]]) = new FixPt[S,I,F](s)
  @api def apply[S:BOOL,I:INT,F:INT](x: Int, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: Long, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: Float, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: Double, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: String, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: BigInt, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))
  @api def apply[S:BOOL,I:INT,F:INT](x: BigDecimal, force: Boolean = false): FixPt[S,I,F] = FixPt[S,I,F](const(x, force))


  /** Constants **/
  @internal def literalToBigDecimal[S:BOOL,I:INT,F:INT](x: Any, force: Boolean): BigDecimal = {
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
      case x: String if !x.exists(_ == '.') => makeFixPt(BigDecimal(x))
      case c =>
        error(ctx, s"$c cannot be lifted to a fixed point value")
        error(ctx)
        BigDecimal(0)
    }
  }
  @internal def const[S:BOOL,I:INT,F:INT](x: Any, force: Boolean = true): Const[FixPt[S,I,F]] = {
    constant[FixPt[S,I,F]](literalToBigDecimal[S,I,F](x,force))
  }
  @internal def int32(x: BigDecimal): Const[Int32] = const[TRUE,_32,_0](x, force = false)
  @internal def int64(x: BigDecimal): Const[Int64] = const[TRUE,_64,_0](x, force = false)


  /** Constructors **/
  @internal def neg[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = x match {
    case Const(c:BigDecimal) => const[S,I,F](-c)
    case Op(FixNeg(x)) => x
    case _ => stage(FixNeg(x))(ctx)
  }
  @internal def inv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = x match {
    case Const(c:BigDecimal) if c.isWhole => const[S,I,F](BigDecimal(~c.toBigInt))
    case Op(FixInv(x)) => x
    case _ => stage(FixInv(x))(ctx)
  }
  @internal def add[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[S,I,F](a + b)
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
  @internal def lt[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Bool.const(a < b)
    case _ => stage( FixLt(x,y))(ctx)
  }

  @internal def leq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Bool.const(a <= b)
    case _ => stage(FixLeq(x,y))(ctx)
  }
  @internal def neq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Bool.const(a != b)
    case _ => stage(FixNeq(x,y))(ctx)
  }
  @internal def eql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => Bool.const(a == b)
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

  @internal def to_flt[S:BOOL,I:INT,F:INT,G:INT,E:INT](x: Exp[FixPt[_,_,_]]): Exp[FltPt[G,E]] = {
    stage(FixPtToFltPt[S,I,F,G,E](x.asInstanceOf[Exp[FixPt[S,I,F]]]))(ctx)
  }

  @internal def from_text[S:BOOL,I:INT,F:INT](x: Exp[Text]): Exp[FixPt[S,I,F]] = x match {
    case Const(c: String) => FixPt[S,I,F](c).s
    case _ => stage(StringToFixPt[S,I,F](x))(ctx)
  }
}

/** Static methods and implicits **/
trait FixPtExp {
  /** Type Aliases **/
  type Index = FixPt[TRUE,_32,_0]
  type Int64 = FixPt[TRUE,_64,_0]
  type Int32 = FixPt[TRUE,_32,_0]
  type Int16 = FixPt[TRUE,_16,_0]
  type  Int8 = FixPt[TRUE, _8,_0]

  @generate
  type UIntJJ$JJ$2to128 = FixPt[FALSE,_JJ,_0]


  /** Static methods **/
  def isFixPtType(x: Type[_]) = FixPtType.unapply(x).isDefined
  def isInt32Type(x: Type[_]) = IntType.unapply(x)
  @internal def intParam(c: Int): Param[Int32] = parameter[Int32](FixPt.literalToBigDecimal[TRUE,_32,_0](c, force=true))

  /** Type classes **/
  implicit def fixPtIsStaged[S:BOOL,I:INT,F:INT]: Type[FixPt[S,I,F]] = FixPtType[S,I,F](BOOL[S],INT[I],INT[F])
  implicit def fixPtIsNum[S:BOOL,I:INT,F:INT]: Num[FixPt[S,I,F]] = FixPtNum[S,I,F]

  /** Rewrite rules **/
  /*@rewrite def Bool$not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Def(node@FixNeq(a,b)) => stage( FixEql(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixEql(a,b)) => stage( FixNeq(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def( node@FixLt(a,b)) => stage( FixLeq(b,a)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixLeq(a,b)) => stage(  FixLt(b,a)(node.mS,node.mI,node.mF) )(ctx)
  }*/


  /** Lifting **/
  @api implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int): FixPt[S,I,F] = FixPt[S,I,F](x, force = false)
  @api implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long): FixPt[S,I,F] = FixPt[S,I,F](x, force = false)

  implicit object LiftInt extends Lift[Int,Int32] {
    @internal def apply(x: Int): Int32 = int2fixpt(x)
  }
  implicit object LiftLong extends Lift[Long,Int64] {
    @internal def apply(x: Long): Int64 = long2fixpt(x)
  }

  /** Casting **/
  implicit def int_cast_fixpt[S:BOOL,I:INT,F:INT]: Cast[Int,FixPt[S,I,F]] = new Cast[Int,FixPt[S,I,F]] {
    @internal def apply(x: Int): FixPt[S,I,F] = FixPt[S,I,F](x, force=true)
  }
  implicit def long_cast_fixpt[S:BOOL,I:INT,F:INT]: Cast[Long,FixPt[S,I,F]] = new Cast[Long,FixPt[S,I,F]] {
    @internal def apply(x: Long): FixPt[S,I,F] = FixPt[S,I,F](x, force=true)
  }
  implicit def fixpt2fixpt[S:BOOL,I:INT,F:INT, S2:BOOL,I2:INT,F2:INT] = new Cast[FixPt[S,I,F],FixPt[S2,I2,F2]] {
    @internal def apply(x: FixPt[S,I,F]): FixPt[S2,I2,F2] = wrap(FixPt.convert[S,I,F,S2,I2,F2](x.s))
  }
  implicit def fixpt2fltpt[S:BOOL,I:INT,F:INT, G:INT,E:INT] = new Cast[FixPt[S,I,F],FltPt[G,E]] {
    @internal def apply(x: FixPt[S,I,F]): FltPt[G,E] = wrap(FixPt.to_flt[S,I,F,G,E](x.s))
  }
  implicit def text2fixpt[S:BOOL,I:INT,F:INT] = new Cast[Text,FixPt[S,I,F]] {
    @internal def apply(x: Text): FixPt[S,I,F] = wrap(FixPt.from_text[S,I,F](x.s))
  }
}

trait FixPtApi {
  type Long  = FixPt[TRUE,_64,_0]
  type Int   = FixPt[TRUE,_32,_0]
  type Short = FixPt[TRUE,_16,_0]
  type Char  = FixPt[TRUE, _8,_0]
}



/** IR Nodes **/
abstract class FixPtOp[S:BOOL,I:INT,F:INT,R:Type] extends Op[R] {
  protected val fix = FixPt
  def mS = BOOL[S]
  def mI = INT[I]
  def mF = INT[F]
  def tp = FixPtType[S,I,F]
}
abstract class FixPtOp1[S:BOOL,I:INT,F:INT] extends FixPtOp[S,I,F,FixPt[S,I,F]]

case class FixInv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.inv(f(x)) }
case class FixNeg[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.neg(f(x)) }

case class FixAdd[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.add(f(x), f(y)) }
case class SatAdd[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.add_sat(f(x), f(y)) }
case class FixSub[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.sub(f(x), f(y)) }
case class SatSub[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.sub_sat(f(x), f(y)) }
case class FixMul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.mul(f(x), f(y)) }
case class UnbSatMul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.mul_unb_sat(f(x), f(y)) }
case class SatMul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.mul_sat(f(x), f(y)) }
case class UnbMul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.mul_unbias(f(x), f(y)) }
case class FixDiv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.div(f(x), f(y)) }
case class UnbSatDiv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.div_unb_sat(f(x), f(y)) }
case class SatDiv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.div_sat(f(x), f(y)) }
case class UnbDiv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.div_unbias(f(x), f(y)) }
case class FixAnd[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.and(f(x), f(y)) }
case class FixOr [S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) =  fix.or(f(x), f(y)) }
case class FixXor [S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) =  fix.xor(f(x), f(y)) }
case class FixLt [S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,Bool] { def mirror(f:Tx) = fix.lt(f(x), f(y)) }
case class FixLeq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,Bool] { def mirror(f:Tx) = fix.leq(f(x), f(y)) }
case class FixNeq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,Bool] { def mirror(f:Tx) = fix.neq(f(x), f(y)) }
case class FixEql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,Bool] { def mirror(f:Tx) = fix.eql(f(x), f(y)) }
case class FixMod[S:BOOL,I:INT](x: Exp[FixPt[S,I,_0]], y: Exp[FixPt[S,I,_0]]) extends FixPtOp1[S,I,_0] { def mirror(f:Tx) = fix.mod(f(x), f(y)) }

case class FixLsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.lsh(f(x), f(y)) }
case class FixRsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.rsh(f(x), f(y)) }
case class FixURsh[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,_0]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.ursh(f(x), f(y)) }

case class FixRandom[S:BOOL,I:INT,F:INT](max: Option[Exp[FixPt[S,I,F]]]) extends FixPtOp1[S,I,F] { def mirror(f:Tx) = fix.random[S,I,F](f(max)) }

case class FixConvert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Exp[FixPt[S,I,F]]) extends FixPtOp1[S2,I2,F2] {
  def mirror(f:Tx) = fix.convert[S,I,F,S2,I2,F2](f(x))
}

case class FixPtToFltPt[S:BOOL,I:INT,F:INT,G:INT,E:INT](x: Exp[FixPt[S,I,F]]) extends FltPtOp1[G,E] {
  def mirror(f:Tx) = FixPt.to_flt[S,I,F,G,E](f(x))
}

case class StringToFixPt[S:BOOL,I:INT,F:INT](x: Exp[Text]) extends FixPtOp1[S,I,F] {
  def mirror(f:Tx) = fix.from_text[S,I,F](f(x))
}



