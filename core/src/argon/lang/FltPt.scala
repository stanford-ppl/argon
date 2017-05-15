package argon.lang

import argon._
import argon.core.UserFacing
import argon.typeclasses._
import forge._

case class FltPt[G:INT,E:INT](s: Exp[FltPt[G,E]]) extends MetaAny[FltPt[G,E]] {
  protected val flt = FltPt
  @api def unary_-(): FltPt[G,E] = FltPt(flt.neg(this.s))
  @api def + (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.add(this.s,that.s))
  @api def - (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.sub(this.s,that.s))
  @api def * (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.mul(this.s,that.s))
  @api def / (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.div(this.s,that.s))
  @api def < (that: FltPt[G,E]): Bool       = Bool( flt.lt(this.s,that.s))
  @api def <=(that: FltPt[G,E]): Bool       = Bool(flt.leq(this.s,that.s))
  @api def > (that: FltPt[G,E]): Bool       = Bool( flt.lt(that.s,this.s))
  @api def >=(that: FltPt[G,E]): Bool       = Bool(flt.leq(that.s,this.s))
  @api def ===(that: FltPt[G,E]): Bool      = Bool(flt.eql(this.s,that.s))
  @api def =!=(that: FltPt[G,E]): Bool      = Bool(flt.neq(this.s,that.s))

  @api override def toText = Text.ify(this)
}

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

  @api def zero = FltPt[G,E](0, force=true)
  @api def one = FltPt[G,E](1, force=true)
  @api def random(max: Option[FltPt[G,E]]): FltPt[G,E] = FltPt(FltPt.random[G,E](max.map(_.s)))
  @api def length: Int = INT[G].v + INT[E].v

  @api def lessThan(x: FltPt[G,E], y: FltPt[G,E]) = x < y
  @api def lessThanOrEqual(x: FltPt[G,E], y: FltPt[G,E]) = x <= y
  @api def equal(x: FltPt[G,E], y: FltPt[G,E]) = x === y

  @api def toFixPt[S:BOOL,I:INT,F:INT](x: FltPt[G,E]): FixPt[S,I,F] = FixPt(FltPt.to_fix[G,E,S,I,F](x.s))
  @api def toFltPt[G2:INT,E2:INT](x: FltPt[G,E]): FltPt[G2,E2] = FltPt(FltPt.convert[G,E,G2,E2](x.s))

  @api def fromInt(x: Int, force: Boolean = true) = FltPt(x, force)
  @api def fromLong(x: Long, force: Boolean = true) = FltPt[G,E](x, force)
  @api def fromFloat(x: Float, force: Boolean = true) = FltPt[G,E](x, force)
  @api def fromDouble(x: Double, force: Boolean = true) = FltPt[G,E](x, force)
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

object FltPt {
  /** Static methods **/
  @internal def apply[G:INT,E:INT](s: Exp[FltPt[G,E]]): FltPt[G,E] = new FltPt[G,E](s)
  @api def apply[G:INT,E:INT](x: Int, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))
  @api def apply[G:INT,E:INT](x: Long, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))
  @api def apply[G:INT,E:INT](x: Float, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))
  @api def apply[G:INT,E:INT](x: Double, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))
  @api def apply[G:INT,E:INT](x: String, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))
  @api def apply[G:INT,E:INT](x: BigInt, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))
  @api def apply[G:INT,E:INT](x: BigDecimal, force: Boolean = false): FltPt[G,E] = FltPt[G,E](s=const(x, force))


  /** Constants **/
  @internal def literalToBigDecimal[G:INT,E:INT](x: Any, force: Boolean): BigDecimal = {
    val gbits = INT[G].v
    val ebits = INT[E].v
    val tp = s"$gbits.$ebits floating point"
    val FLP = FltPtType[G,E].toString

    // TODO: Precision checking
    def makeFloat(v: BigDecimal): BigDecimal = v

    x match {
      case x: BigDecimal => makeFloat(x)
      case x: Int => makeFloat(BigDecimal(x))
      case x: Long => makeFloat(BigDecimal(x))
      case x: Float => makeFloat(BigDecimal(x.toDouble))
      case x: Double => makeFloat(BigDecimal(x))
      case x: String => makeFloat(BigDecimal(x))
      case c =>
        error(s"$c cannot be lifted to a floating point value")
        sys.exit()
    }
  }
  @internal def const[G:INT,E:INT](x: Any, force: Boolean = true): Const[FltPt[G,E]] = {
    constant(literalToBigDecimal[G,E](x, force))
  }

  @internal def string2fltpt[G:INT,E:INT](x: String): FltPt[G,E] = FltPt(const[G,E](x))


  /** Constructors **/
  @internal def neg[G:INT,E:INT](x: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = x match {
    case Const(c:BigDecimal) => const[G,E](-c)
    case Op(FltNeg(x)) => x
    case _ => stage(FltNeg(x))(ctx)
  }
  @internal def add[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[G,E](a + b)
    case _ => stage(FltAdd(x,y))(ctx)
  }
  @internal def sub[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[G,E](a - b)
    case _ => stage(FltSub(x,y))(ctx)
  }
  @internal def mul[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[G,E](a * b)
    case _ => stage(FltMul(x,y))(ctx)
  }
  @internal def div[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => const[G,E](a / b)
    case _ => stage(FltDiv(x,y))(ctx)
  }
  @internal def lt[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => Bool.const(a < b)
    case _ => stage(FltLt(x,y))(ctx)
  }
  @internal def leq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => Bool.const(a <= b)
    case _ => stage(FltLeq(x,y))(ctx)
  }
  @internal def neq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => Bool.const(a != b)
    case _ => stage(FltNeq(x,y))(ctx)
  }
  @internal def eql[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => Bool.const(a == b)
    case _ => stage(FltEql(x,y))(ctx)
  }
  @internal def random[G:INT,E:INT](max: Option[Exp[FltPt[G,E]]]): Exp[FltPt[G,E]] = {
    stageSimple(FltRandom[G,E](max))(ctx)
  }

  @internal def convert[G:INT,E:INT,G2:INT,E2:INT](x: Exp[FltPt[_,_]]): Exp[FltPt[G2,E2]] = {
    stage(FltConvert[G,E,G2,E2](x.asInstanceOf[Exp[FltPt[G,E]]]))(ctx)
  }
  @internal def to_fix[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Exp[FltPt[_,_]]): Exp[FixPt[S,I,F]] = {
    stage(FltPtToFixPt[G,E,S,I,F](x.asInstanceOf[Exp[FltPt[G,E]]]))(ctx)
  }
  @internal def from_text[G:INT,E:INT](x: Exp[Text]): Exp[FltPt[G,E]] = x match {
    case Const(c: String) => string2fltpt[G,E](c).s
    case _ => stage(StringToFltPt[G,E](x))(ctx)
  }
}

trait FltPtExp {
  /** Type aliases **/
  type Float64 = FltPt[_53,_11]
  type Float32 = FltPt[_24, _8]
  type Float16 = FltPt[_11, _5]

  /** Static methods **/
  def isFltPtType(x: Type[_]) = FltPtType.unapply(x).isDefined


  /** Rewrite rules **/
  /*@rewrite def Bool$not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Op(node@FltNeq(a,b)) => stage( FltEql(a,b)(node.mG,node.mE) )(ctx)
    case Op(node@FltEql(a,b)) => stage( FltNeq(a,b)(node.mG,node.mE) )(ctx)
    case Op( node@FltLt(a,b)) => stage( FltLeq(b,a)(node.mG,node.mE) )(ctx)
    case Op(node@FltLeq(a,b)) => stage(  FltLt(b,a)(node.mG,node.mE) )(ctx)
  }*/

  /** Type classes **/
  implicit def fltPtIsStaged[G:INT,E:INT]: Type[FltPt[G,E]] = FltPtType(INT[G],INT[E])
  implicit def fltPtIsNum[G:INT,E:INT]: Num[FltPt[G,E]] = new FltPtNum[G,E]


  /** Lifting **/
  implicit object Float2FltPt extends Lift[Float,Float32] {
    @internal def apply(x: Float): Float32 = float2fltpt(x)
  }
  implicit object Double2FltPt extends Lift[Double,Float64] {
    @internal def apply(x: Double): Float64 = double2fltpt(x)
  }

  @api implicit def int2fltpt[G:INT,E:INT](x: Int): FltPt[G,E] = FltPt[G,E](x, force=false)
  @api implicit def long2fltpt[G:INT,E:INT](x: Long): FltPt[G,E] = FltPt[G,E](x, force=false)
  @api implicit def float2fltpt[G:INT,E:INT](x: Float): FltPt[G,E] = FltPt[G,E](x, force=false)
  @api implicit def double2fltpt[G:INT,E:INT](x: Double): FltPt[G,E] = FltPt[G,E](x, force=false)


  /** Casting **/
  implicit def int_cast_fltpt[G:INT,E:INT]: Cast[Int,FltPt[G,E]] = new Cast[Int,FltPt[G,E]] {
    @internal def apply(x: Int): FltPt[G,E] = int2fltpt[G,E](x)
  }
  implicit def long_cast_fltpt[G:INT,E:INT]: Cast[Long,FltPt[G,E]] = new Cast[Long,FltPt[G,E]] {
    @internal def apply(x: Long): FltPt[G,E] = long2fltpt[G,E](x)
  }
  implicit def float_cast_fltpt[G:INT,E:INT]: Cast[Float,FltPt[G,E]] = new Cast[Float,FltPt[G,E]] {
    @internal def apply(x: Float): FltPt[G,E] = float2fltpt[G,E](x)
  }
  implicit def double_cast_fltpt[G:INT,E:INT]: Cast[Double,FltPt[G,E]] = new Cast[Double,FltPt[G,E]] {
    @internal def apply(x: Double): FltPt[G,E] = double2fltpt[G,E](x)
  }

  implicit def fltpt2fltpt[G:INT,E:INT, G2:INT,E2:INT] = new Cast[FltPt[G,E],FltPt[G2,E2]] {
    @internal def apply(x: FltPt[G,E]): FltPt[G2,E2] = FltPt(FltPt.convert[G,E,G2,E2](x.s))
  }
  implicit def fltpt2fixpt[G:INT,E:INT,S:BOOL,I:INT,F:INT] = new Cast[FltPt[G,E],FixPt[S,I,F]] {
    @internal def apply(x: FltPt[G,E]): FixPt[S,I,F] = FixPt(FltPt.to_fix[G,E,S,I,F](x.s))
  }
  implicit def text2fltpt[G:INT,E:INT] = new Cast[Text,FltPt[G,E]] {
    @internal def apply(x: Text): FltPt[G,E] = FltPt(FltPt.from_text[G,E](x.s))
  }
}

trait FltPtApi {
  type Double = FltPt[_53,_11]
  type Float  = FltPt[_24, _8]
  type Half   = FltPt[_11, _5]
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
case class FltLt [G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,Bool] { def mirror(f:Tx) = flt.lt(f(x), f(y)) }
case class FltLeq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,Bool] { def mirror(f:Tx) = flt.leq(f(x), f(y)) }
case class FltNeq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,Bool] { def mirror(f:Tx) = flt.neq(f(x), f(y)) }
case class FltEql[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E,Bool] { def mirror(f:Tx) = flt.eql(f(x), f(y)) }

case class FltRandom[G:INT,E:INT](max: Option[Exp[FltPt[G,E]]]) extends FltPtOp1[G,E] { def mirror(f:Tx) = flt.random[G,E](f(max)) }

case class FltConvert[G:INT,E:INT,G2:INT,E2:INT](x: Exp[FltPt[G,E]]) extends FltPtOp1[G2,E2] {
  def mirror(f:Tx) = flt.convert[G,E,G2,E2](f(x))
}
case class FltPtToFixPt[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Exp[FltPt[G,E]]) extends FixPtOp1[S,I,F] {
  def mirror(f:Tx) = FltPt.to_fix[G,E,S,I,F](f(x))
}
case class StringToFltPt[G:INT,E:INT](x: Exp[Text]) extends FltPtOp1[G,E] {
  def mirror(f:Tx) = flt.from_text[G,E](f(x))
}

