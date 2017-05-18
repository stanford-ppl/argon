package argon.nodes

import argon._
import argon.core.UserFacing
import forge._

sealed class FixPtType[S,I,F](val mS: BOOL[S], val mI: INT[I], val mF: INT[F]) extends Type[FixPt[S,I,F]] with CanBits[FixPt[S,I,F]] {
  def wrapped(s: Exp[FixPt[S,I,F]]): FixPt[S,I,F] = FixPt[S,I,F](s)(mS,mI,mF)
  def stagedClass = classOf[FixPt[S,I,F]]
  def isPrimitive = true

  def isSigned: CBoolean = mS.v
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

  @api def zero = FixPt.lift[S,I,F](0,force=true)
  @api def one = FixPt.lift[S,I,F](1,force=true)
  @api def random(max: Option[FixPt[S,I,F]]): FixPt[S,I,F] = FixPt(FixPt.random[S,I,F](max.map(_.s)))
  def length: Int = INT[I].v + INT[F].v

  @api def lessThan(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x < y
  @api def lessThanOrEqual(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x <= y
  @api def equal(x: FixPt[S,I,F], y: FixPt[S,I,F]) = x === y

  @api def toFixPt[S2:BOOL,I2:INT,F2:INT](x: FixPt[S,I,F]): FixPt[S2,I2,F2] = FixPt(FixPt.convert[S,I,F,S2,I2,F2](x.s))
  @api def toFltPt[G:INT,E:INT](x: FixPt[S,I,F]): FltPt[G,E] = FltPt(FixPt.to_flt[S,I,F,G,E](x.s))

  @api def fromInt(x: Int, force: CBoolean = true) = FixPt.lift[S,I,F](x, force)
  @api def fromLong(x: Long, force: CBoolean = true) = FixPt.lift[S,I,F](x, force)
  @api def fromFloat(x: Float, force: CBoolean = true) = FixPt.lift[S,I,F](x, force)
  @api def fromDouble(x: Double, force: CBoolean = true) = FixPt.lift[S,I,F](x, force)
}

object FixPtNum {
  def apply[S:BOOL,I:INT,F:INT] = new FixPtNum[S,I,F]
}

object FixPtType {
  def apply[S:BOOL,I:INT,F:INT] = new FixPtType[S,I,F](BOOL[S],INT[I],INT[F])

  def unapply(x:Type[_]):Option[(CBoolean, Int, Int)] = x match {
    case tp:FixPtType[_,_,_] => Some((tp.isSigned, tp.intBits, tp.fracBits))
    case _ => None
  }
}

object IntType extends FixPtType(BOOL[TRUE],INT[_32],INT[_0]) with UserFacing {
  def unapply(x: Type[_]): CBoolean = x match {
    case FixPtType(true, 32, 0) => true
    case _ => false
  }
  override def toStringUser = "Int"
}

object LongType extends FixPtType(BOOL[TRUE],INT[_64],INT[_0]) with UserFacing {
  def unapply(x: Type[_]): CBoolean = x match {
    case FixPtType(true, 64, 0) => true
    case _ => false
  }
  override def toStringUser = "Long"
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
case class FixLt [S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,MBoolean] { def mirror(f:Tx) = fix.lt(f(x), f(y)) }
case class FixLeq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,MBoolean] { def mirror(f:Tx) = fix.leq(f(x), f(y)) }
case class FixNeq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,MBoolean] { def mirror(f:Tx) = fix.neq(f(x), f(y)) }
case class FixEql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F,MBoolean] { def mirror(f:Tx) = fix.eql(f(x), f(y)) }
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

case class StringToFixPt[S:BOOL,I:INT,F:INT](x: Exp[MString]) extends FixPtOp1[S,I,F] {
  def mirror(f:Tx) = fix.from_text[S,I,F](f(x))
}

