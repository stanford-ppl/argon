package argon.ops

import argon.core.{ArgonExceptions, Staging}
import argon.typeclasses._

trait FltPtApi extends FltPtExp with BoolApi with BitsApi with NumApi with OrderApi with CastApi { this: TextApi =>
  type Double = Float64
  type Float = Float32
  type Half = Float16
}

trait FltPtExp extends Staging with BoolExp with BitsExp with NumExp with OrderExp with CastExp with CustomBitWidths with ArgonExceptions {
  this: TextExp =>

  /** Type aliases **/
  type Float16 = FltPt[_11,_5]
  type Float32 = FltPt[_24,_8]
  type Float64 = FltPt[_53,_11]

  /** Infix methods **/
  case class FltPt[G:INT,E:INT](s: Exp[FltPt[G,E]]) {
    def unary_-(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(flt_neg(this.s))
    def + (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(flt_add(this.s,that.s))
    def - (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(flt_sub(this.s,that.s))
    def * (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(flt_mul(this.s,that.s))
    def / (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(flt_div(this.s,that.s))
    def < (that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = Bool( flt_lt(this.s,that.s))
    def <=(that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = Bool(flt_leq(this.s,that.s))
    def > (that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = Bool( flt_lt(that.s,this.s))
    def >=(that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = Bool(flt_leq(that.s,this.s))

    def +(rhs: Text)(implicit ctx: SrcCtx): Text = textify(this) + lift(rhs)
    def +(rhs: String)(implicit ctx: SrcCtx): Text = this + lift[String,Text](rhs)
  }

  /** Virtualized methods **/
  def infix_!=[G:INT,E:INT](x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = Bool(flt_neq(x.s,y.s))
  def infix_==[G:INT,E:INT](x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = Bool(flt_eql(x.s,y.s))
  def infix_toString[G:INT,E:INT](x: FltPt[G,E])(implicit ctx: SrcCtx): Text = textify(x)


  /** Type classes **/
  // --- FStaged
  class FltPtType[G,E](val mG: INT[G], val mE: INT[E]) extends FStaged[FltPt[G,E]] {
    override def wrapped(s: Exp[FltPt[G,E]]): FltPt[G,E] = FltPt[G,E](s)(mG,mE)
    override def unwrapped(x: FltPt[G,E]) = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[FltPt[G,E]]
    override def isPrimitive = true

    override def hashCode() = (sigBits, expBits).##
    override def equals(x: Any) = x match {
      case t:FltPtType[_,_] => t.mG == this.mG && t.mE == this.mE
      case _ => false
    }

    def sigBits: Int = mG.v
    def expBits: Int = mE.v
  }
  implicit def fltPtType[G:INT,E:INT]: FStaged[FltPt[G,E]] = new FltPtType(INT[G],INT[E])

  def isFltPtType(x: FStaged[_]) = FltPtType.unapply(x).isDefined

  object FltPtType {
    def unapply(x:BStaged[_]):Option[(Int, Int)] = x match {
      case tp:FltPtType[_, _] => Some((tp.sigBits, tp.expBits))
      case _ => None
    }
  }

  object FloatType extends FltPtType(INT[_24],INT[_8]) {
    def unapply(x: BStaged[_]): Boolean = x match {
      case FltPtType(24, 8) => true
      case _ => false
    }
  }
  object DoubleType extends FltPtType(INT[_53],INT[_11]) {
    def unapply(x: BStaged[_]): Boolean = x match {
      case FltPtType(53, 11) => true
      case _ => false
    }
  }

  // --- Num
  class FltPtNum[G:INT,E:INT] extends Num[FltPt[G,E]] {
    override def negate(x: FltPt[G,E])(implicit ctx: SrcCtx) = -x
    override def plus(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x + y
    override def minus(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x - y
    override def times(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x * y
    override def divide(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x / y

    override def zero(implicit ctx: SrcCtx) = int2fltpt[G,E](0)
    override def one(implicit ctx: SrcCtx) = int2fltpt[G,E](1)
    override def random(max: Option[FltPt[G,E]])(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(flt_random[G,E](max.map(_.s)))
    override def length: Int = INT[G].v + INT[E].v

    override def lessThan(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x < y
    override def lessThanOrEqual(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x <= y
    override def equal(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = infix_==(x, y)
  }
  implicit def __fltPtNum[G:INT,E:INT]: Num[FltPt[G,E]] = new FltPtNum[G,E]

  override protected def bitsUnapply[T](tp: FStaged[T]): Option[Bits[T]] = tp match {
    case tp: FltPtType[_,_] => Some(new FltPtNum()(tp.mG,tp.mE).asInstanceOf[Bits[T]])
    case _ => super.bitsUnapply(tp)
  }


  // --- Lift
  implicit object Float2FltPt extends Lift[Float,Float32] { val FStaged = fltPtType[_24,_8] }
  implicit object Double2FltPt extends Lift[Double,Float64] { val FStaged = fltPtType[_53,_11] }


  /** Constant lifting **/
  private def createConstant[G:INT,E:INT](x: Any, enWarn: Boolean = true)(implicit ctx: SrcCtx): Const[FltPt[G,E]] = {
    val gbits = INT[G].v
    val ebits = INT[E].v
    val tp = s"$gbits.$ebits floating point"
    val FLP = readable(fltPtType[G,E])

    def makeFloat(v: BigDecimal): Const[FltPt[G,E]] = {
      // TODO: Precision checking
      constant[FltPt[G,E]](v)
    }

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

  implicit def int2fltpt[G:INT,E:INT](x: Int)(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(createConstant[G,E](x))
  implicit def long2fltpt[G:INT,E:INT](x: Long)(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(createConstant[G,E](x))
  implicit def float2fltpt[G:INT,E:INT](x: Float)(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(createConstant[G,E](x))
  implicit def double2fltpt[G:INT,E:INT](x: Double)(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(createConstant[G,E](x))
  def string2fltpt[G:INT,E:INT](x: String)(implicit ctx: SrcCtx): FltPt[G,E] = FltPt(createConstant[G,E](x))

  override def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l match {
    case Float2FltPt => FltPt(createConstant[_24,_8](x)).asInstanceOf[B]
    case Double2FltPt => FltPt(createConstant[_53,_11](x)).asInstanceOf[B]
    case _ => super.__lift(x)
  }

  def fltpt[G:INT,E:INT](x: BigDecimal)(implicit ctx: SrcCtx): Const[FltPt[G,E]] = createConstant[G,E](x, enWarn=false)

  /** Lifting methods **/
  implicit class IntFltPtOps(x: Int) {
    private def lift[G:INT,E:INT](implicit ctx: SrcCtx): FltPt[G,E] = int2fltpt[G,E](x)
    def + [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] + y
    def - [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] - y
    def * [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] * y
    def / [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] / y
    def < [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] < y
    def <=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] <= y
    def > [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] > y
    def >=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] >= y
  }
  implicit class LongFltPtOps(x: Long) {
    private def lift[G:INT,E:INT](implicit ctx: SrcCtx): FltPt[G,E] = long2fltpt[G,E](x)
    def + [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] + y
    def - [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] - y
    def * [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] * y
    def / [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] / y
    def < [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] < y
    def <=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] <= y
    def > [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] > y
    def >=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] >= y
  }
  implicit class FloatFltPtOps(x: Float) {
    private def lift[G:INT,E:INT](implicit ctx: SrcCtx): FltPt[G,E] = float2fltpt[G,E](x)
    def + [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] + y
    def - [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] - y
    def * [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] * y
    def / [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] / y
    def < [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] < y
    def <=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] <= y
    def > [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] > y
    def >=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] >= y
  }
  implicit class DoubleFltPtOps(x: Double) {
    private def lift[G:INT,E:INT](implicit ctx: SrcCtx): FltPt[G,E] = double2fltpt[G,E](x)
    def + [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] + y
    def - [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] - y
    def * [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] * y
    def / [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] / y
    def < [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] < y
    def <=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] <= y
    def > [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] > y
    def >=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] >= y
  }


  /** Casting **/
  override protected def cast[T:FStaged:Num,R:FStaged:Num](x: T)(implicit ctx: SrcCtx): R = (ftyp[T],ftyp[R]) match {
    case (a:FltPtType[g,e],b:FltPtType[g2,e2]) =>
      // Why are these asInstanceOfs necessary here??
      implicit val mG: INT[g] = a.mG.asInstanceOf[INT[g]]
      implicit val mE: INT[e] = a.mE.asInstanceOf[INT[e]]
      implicit val mG2: INT[g2] = b.mG.asInstanceOf[INT[g2]]
      implicit val mE2: INT[e2] = b.mE.asInstanceOf[INT[e2]]

      b.wrapped( flt_convert[g,e,g2,e2](x.asInstanceOf[FltPt[g,e]].s) ).asInstanceOf[R]

    case _ => super.cast[T,R](x)
  }

  override protected def castLift[R:FStaged:Num](x: Any)(implicit ctx: SrcCtx): R = ftyp[R] match {
    case tp:FltPtType[g,e] =>
      implicit val mG: INT[g] = tp.mG.asInstanceOf[INT[g]]
      implicit val mE: INT[e] = tp.mE.asInstanceOf[INT[e]]

      FltPt(createConstant[g,e](x)).asInstanceOf[R]
    case _ => super.castLift[R](x)
  }



  /** Nodes **/
  abstract class FltPtOp[G:INT,E:INT] extends Op[FltPt[G,E]] {
    def mG = INT[G]
    def mE = INT[E]
    def tp = fltPtType[G,E]
  }
  abstract class FltPtOp2[G:INT,E:INT,R:FStaged] extends Op[R] {
    def mG = INT[G]
    def mE = INT[E]
    def tp = fltPtType[G,E]
  }

  case class FltNeg[G:INT,E:INT](x: Exp[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = flt_neg(f(x)) }

  case class FltAdd[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = flt_add(f(x), f(y)) }
  case class FltSub[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = flt_sub(f(x), f(y)) }
  case class FltMul[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = flt_mul(f(x), f(y)) }
  case class FltDiv[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = flt_div(f(x), f(y)) }
  case class FltLt [G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = flt_lt(f(x), f(y)) }
  case class FltLeq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = flt_leq(f(x), f(y)) }
  case class FltNeq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = flt_neq(f(x), f(y)) }
  case class FltEql[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = flt_eql(f(x), f(y)) }

  case class FltRandom[G:INT,E:INT](max: Option[Exp[FltPt[G,E]]]) extends FltPtOp[G,E] { def mirror(f:Tx) = flt_random[G,E](f(max)) }

  case class FltConvert[G:INT,E:INT,G2:INT,E2:INT](x: Exp[FltPt[G,E]]) extends FltPtOp[G2,E2] {
    def mirror(f:Tx) = flt_convert[G,E,G2,E2](x)
  }


  /** Constructors **/
  def flt_neg[G:INT,E:INT](x: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = x match {
    case Const(c:BigDecimal) => fltpt[G,E](-c)
    case Op(FltNeg(x)) => x
    case _ => stage(FltNeg(x))(ctx)
  }
  def flt_add[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => fltpt[G,E](a + b)
    case _ => stage(FltAdd(x,y))(ctx)
  }
  def flt_sub[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => fltpt[G,E](a - b)
    case _ => stage(FltSub(x,y))(ctx)
  }
  def flt_mul[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => fltpt[G,E](a * b)
    case _ => stage(FltMul(x,y))(ctx)
  }
  def flt_div[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => fltpt[G,E](a / b)
    case _ => stage(FltDiv(x,y))(ctx)
  }
  def flt_lt[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => bool(a < b)
    case _ => stage(FltLt(x,y))(ctx)
  }
  def flt_leq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => bool(a <= b)
    case _ => stage(FltLeq(x,y))(ctx)
  }
  def flt_neq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => bool(a != b)
    case _ => stage(FltNeq(x,y))(ctx)
  }
  def flt_eql[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => bool(a == b)
    case _ => stage(FltEql(x,y))(ctx)
  }
  def flt_random[G:INT,E:INT](max: Option[Exp[FltPt[G,E]]])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = {
    stageSimple(FltRandom[G,E](max))(ctx)
  }

  def flt_convert[G:INT,E:INT,G2:INT,E2:INT](x: Exp[FltPt[_,_]])(implicit ctx: SrcCtx): Exp[FltPt[G2,E2]] = {
    stage(FltConvert[G,E,G2,E2](x.asInstanceOf[Exp[FltPt[G,E]]]))(ctx)
  }


  /** Other rewrite rules **/
  override def bool_not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Op(node@FltNeq(a,b)) => stage( FltEql(a,b)(node.mG,node.mE) )(ctx)
    case Op(node@FltEql(a,b)) => stage( FltNeq(a,b)(node.mG,node.mE) )(ctx)
    case Op( node@FltLt(a,b)) => stage( FltLeq(a,b)(node.mG,node.mE) )(ctx)
    case Op(node@FltLeq(a,b)) => stage(  FltLt(a,b)(node.mG,node.mE) )(ctx)
    case _ => super.bool_not(x)
  }


  /** Internal methods **/
  override def readable(x: Any): String = x match {
    case FltPtType(gbits,ebits) => s"FltPt[$gbits,$ebits]"
    case _ => super.readable(x)
  }
  override def userReadable(x: Any): String = x match {
    case DoubleType() => "Double"
    case FloatType()  => "Float"
    case tp:FltPtType[_,_] => u"FltPt[${tp.mG},${tp.mE}]"
    case _ => super.userReadable(x)
  }
}