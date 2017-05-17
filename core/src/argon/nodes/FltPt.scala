package argon.nodes

import argon._
import argon.core.UserFacing
import argon.typeclasses._
import forge._

class FltPtType[G,E](val mG: INT[G], val mE: INT[E]) extends Type[FltPt[G,E]] with CanBits[FltPt[G,E]] {
  def wrapped(s: Exp[FltPt[G,E]]): FltPt[G,E] = FltPt[G,E](s)(mG,mE)
  def stagedClass = classOf[FltPt[G,E]]
  def isPrimitive = true

  override def hashCode() = (sigBits, expBits).##
  override def equals(x: Any) = x match {
    case t:FltPtType[_,_] => t.mG == this.mG && t.mE == this.mE
    case _ => false
  }

  def sigBits: Int = mG.v
  def expBits: Int = mE.v
  protected def getBits(children: Seq[Type[_]]) = Some(FltPtNum[G,E](mG,mE))

  override def toString = s"FltPt[$mG,$mE]"
}

class FltPtNum[G:INT,E:INT] extends Num[FltPt[G,E]] {
  @api def negate(x: FltPt[G,E]) = -x
  @api def plus(x: FltPt[G,E], y: FltPt[G,E]) = x + y
  @api def minus(x: FltPt[G,E], y: FltPt[G,E]) = x - y
  @api def times(x: FltPt[G,E], y: FltPt[G,E]) = x * y
  @api def divide(x: FltPt[G,E], y: FltPt[G,E]) = x / y

  @api def zero: FltPt[G,E] = FltPt(FltPt.const[G,E](0))
  @api def one: FltPt[G,E] = FltPt(FltPt.const[G,E](1))
  @api def random(max: Option[FltPt[G,E]]): FltPt[G,E] = FltPt(FltPt.random[G,E](max.map(_.s)))
  @api def length: Int = INT[G].v + INT[E].v

  @api def lessThan(x: FltPt[G,E], y: FltPt[G,E]) = x < y
  @api def lessThanOrEqual(x: FltPt[G,E], y: FltPt[G,E]) = x <= y
  @api def equal(x: FltPt[G,E], y: FltPt[G,E]) = x === y

  @api def toFixPt[S:BOOL,I:INT,F:INT](x: FltPt[G,E]): FixPt[S,I,F] = FixPt(FltPt.to_fix[G,E,S,I,F](x.s))
  @api def toFltPt[G2:INT,E2:INT](x: FltPt[G,E]): FltPt[G2,E2] = FltPt(FltPt.convert[G,E,G2,E2](x.s))

  @api def fromInt(x: Int, force: CBoolean = true) = FltPt[G,E](x, force)
  @api def fromLong(x: Long, force: CBoolean = true) = FltPt[G,E](x, force)
  @api def fromFloat(x: Float, force: CBoolean = true) = FltPt[G,E](x, force)
  @api def fromDouble(x: Double, force: CBoolean = true) = FltPt[G,E](x, force)
}

object FltPtNum {
  def apply[G:INT,E:INT] = new FltPtNum[G,E]
}

object FltPtType {
  def apply[G:INT,E:INT]: Type[FltPt[G,E]] = new FltPtType(INT[G],INT[E])

  def unapply(x:Type[_]):Option[(Int, Int)] = x match {
    case tp:FltPtType[_, _] => Some((tp.sigBits, tp.expBits))
    case _ => None
  }
}

object FloatType extends FltPtType(INT[_24],INT[_8]) with UserFacing {
  def unapply(x: Type[_]): Boolean = x match {
    case FltPtType(24, 8) => true
    case _ => false
  }
  override def toStringUser = "Float"
}
object DoubleType extends FltPtType(INT[_53],INT[_11]) with UserFacing {
  def unapply(x: Type[_]): Boolean = x match {
    case FltPtType(53, 11) => true
    case _ => false
  }
  override def toStringUser = "Double"
}


/** IR Nodes **/
abstract class FltPtOp[G:INT,E:INT,R:Type] extends Op[R] {
  protected val flt = FltPt
  def mG = INT[G]
  def mE = INT[E]
  def tp = FltPtType[G,E]
}
abstract class FltPtOp1[G:INT,E:INT] extends FltPtOp[G,E,FltPt[G,E]]


case class FltNeg[G:INT,E:INT](x: Exp[FltPt[G,E]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.neg(f(x)) }

case class FltAdd[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.add(f(x), f(y)) }
case class FltSub[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.sub(f(x), f(y)) }
case class FltMul[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.mul(f(x), f(y)) }
case class FltDiv[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.div(f(x), f(y)) }
case class FltLt [G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,MBoolean] { def mirror(f:Tx) = flt.lt(f(x), f(y)) }
case class FltLeq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,MBoolean] { def mirror(f:Tx) = flt.leq(f(x), f(y)) }
case class FltNeq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,MBoolean] { def mirror(f:Tx) = flt.neq(f(x), f(y)) }
case class FltEql[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,MBoolean] { def mirror(f:Tx) = flt.eql(f(x), f(y)) }

case class FltRandom[G:INT,E:INT](max: Option[Exp[FltPt[G,E]]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.random[G,E](f(max)) }

case class FltConvert[G:INT,E:INT,G2:INT,E2:INT](x: Exp[FltPt[G,E]]) extends FltPtOp1[G2,E2] {
  def mirror(f:Tx) = flt.convert[G,E,G2,E2](f(x))
}
case class FltPtToFixPt[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Exp[FltPt[G,E]]) extends FixPtOp1[S,I,F] {
  def mirror(f:Tx) = FltPt.to_fix[G,E,S,I,F](f(x))
}
case class StringToFltPt[G:INT,E:INT](x: Exp[MString]) extends FltPtOp1[G,E] {
  def mirror(f:Tx) = flt.from_string[G,E](f(x))
}

