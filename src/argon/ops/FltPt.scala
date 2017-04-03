package argon.ops

import argon.core.{ArgonExceptions, Staging}
import argon.typeclasses._
import forge._

trait FltPtApi extends FltPtExp with BoolApi with BitsApi with NumApi with OrderApi with CastApi {
  this: TextApi with FixPtExp =>

  type Double = Float64
  type Float = Float32
  type Half = Float16
}

trait FltPtExp extends Staging with BoolExp with BitsExp with NumExp with OrderExp with CastExp with CustomBitWidths with ArgonExceptions {
  this: TextExp with FixPtExp =>

  /** Type aliases **/
  type Float16 = FltPt[_11,_5]
  type Float32 = FltPt[_24,_8]
  type Float64 = FltPt[_53,_11]

  /** Infix methods **/
  case class FltPt[G:INT,E:INT](s: Exp[FltPt[G,E]]) extends MetaAny[FltPt[G,E]] {
    @api def unary_-(): FltPt[G,E] = FltPt(flt_neg(this.s))
    @api def + (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt_add(this.s,that.s))
    @api def - (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt_sub(this.s,that.s))
    @api def * (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt_mul(this.s,that.s))
    @api def / (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt_div(this.s,that.s))
    @api def < (that: FltPt[G,E]): Bool       = Bool( flt_lt(this.s,that.s))
    @api def <=(that: FltPt[G,E]): Bool       = Bool(flt_leq(this.s,that.s))
    @api def > (that: FltPt[G,E]): Bool       = Bool( flt_lt(that.s,this.s))
    @api def >=(that: FltPt[G,E]): Bool       = Bool(flt_leq(that.s,this.s))
    @api def ===(that: FltPt[G,E]): Bool      = Bool(flt_eql(this.s,that.s))
    @api def =!=(that: FltPt[G,E]): Bool      = Bool(flt_neq(this.s,that.s))

    @api override def toText = textify(this)
  }


  /** Type classes **/
  // --- Staged
  class FltPtType[G,E](val mG: INT[G], val mE: INT[E]) extends Meta[FltPt[G,E]] with CanBits[FltPt[G,E]] {
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
    protected def getBits(children: Seq[Type[_]]) = Some(__fltPtNum[G,E](mG,mE))
  }
  implicit def fltPtType[G:INT,E:INT]: Meta[FltPt[G,E]] = new FltPtType(INT[G],INT[E])

  def isFltPtType(x: Type[_]) = FltPtType.unapply(x).isDefined

  object FltPtType {
    def unapply(x:Type[_]):Option[(Int, Int)] = x match {
      case tp:FltPtType[_, _] => Some((tp.sigBits, tp.expBits))
      case _ => None
    }
  }

  object FloatType extends FltPtType(INT[_24],INT[_8]) {
    def unapply(x: Type[_]): Boolean = x match {
      case FltPtType(24, 8) => true
      case _ => false
    }
  }
  object DoubleType extends FltPtType(INT[_53],INT[_11]) {
    def unapply(x: Type[_]): Boolean = x match {
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
    override def equal(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x === y

    def toFixPt[S:BOOL,I:INT,F:INT](x: FltPt[G,E])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(flt_to_fix[G,E,S,I,F](x.s))
    def toFltPt[G2:INT,E2:INT](x: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G2,E2] = FltPt(flt_convert[G,E,G2,E2](x.s))

    def fromInt(x: Int, force: Boolean = true)(implicit ctx: SrcCtx) = FltPt(createConstant(x, !force))
    def fromLong(x: Long, force: Boolean = true)(implicit ctx: SrcCtx) = FltPt(createConstant(x, !force))
    def fromFloat(x: Float, force: Boolean = true)(implicit ctx: SrcCtx) = FltPt(createConstant(x, !force))
    def fromDouble(x: Double, force: Boolean = true)(implicit ctx: SrcCtx) = FltPt(createConstant(x, !force))
  }
  implicit def __fltPtNum[G:INT,E:INT]: Num[FltPt[G,E]] = new FltPtNum[G,E]


  // --- Lift
  implicit object Float2FltPt extends Lift[Float,Float32] {
    def apply(x: Float)(implicit ctx: SrcCtx): Float32 = float2fltpt(x)
  }
  implicit object Double2FltPt extends Lift[Double,Float64] {
    def apply(x: Double)(implicit ctx: SrcCtx): Float64 = double2fltpt(x)
  }


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

  def fltpt[G:INT,E:INT](x: BigDecimal)(implicit ctx: SrcCtx): Const[FltPt[G,E]] = createConstant[G,E](x, enWarn=false)

  /** Casting **/
  implicit def int_cast_fltpt[G:INT,E:INT]: Cast[Int,FltPt[G,E]] = new Cast[Int,FltPt[G,E]] {
    def apply(x: Int)(implicit ctx: SrcCtx): FltPt[G,E] = int2fltpt[G,E](x)
  }
  implicit def long_cast_fltpt[G:INT,E:INT]: Cast[Long,FltPt[G,E]] = new Cast[Long,FltPt[G,E]] {
    def apply(x: Long)(implicit ctx: SrcCtx): FltPt[G,E] = long2fltpt[G,E](x)
  }
  implicit def float_cast_fltpt[G:INT,E:INT]: Cast[Float,FltPt[G,E]] = new Cast[Float,FltPt[G,E]] {
    def apply(x: Float)(implicit ctx: SrcCtx): FltPt[G,E] = float2fltpt[G,E](x)
  }
  implicit def double_cast_fltpt[G:INT,E:INT]: Cast[Double,FltPt[G,E]] = new Cast[Double,FltPt[G,E]] {
    def apply(x: Double)(implicit ctx: SrcCtx): FltPt[G,E] = double2fltpt[G,E](x)
  }

  implicit def fltpt2fltpt[G:INT,E:INT, G2:INT,E2:INT] = new Cast[FltPt[G,E],FltPt[G2,E2]] {
    def apply(x: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G2,E2] = wrap(flt_convert[G,E,G2,E2](x.s))
  }
  implicit def fltpt2fixpt[G:INT,E:INT,S:BOOL,I:INT,F:INT] = new Cast[FltPt[G,E],FixPt[S,I,F]] {
    def apply(x: FltPt[G,E])(implicit ctx: SrcCtx): FixPt[S,I,F] = wrap(flt_to_fix[G,E,S,I,F](x.s))
  }
  implicit def text2fltpt[G:INT,E:INT] = new Cast[Text,FltPt[G,E]] {
    def apply(x: Text)(implicit ctx: SrcCtx): FltPt[G,E] = wrap(text_to_fltpt[G,E](x.s))
  }


  /** Nodes **/
  abstract class FltPtOp[G:INT,E:INT] extends Op[FltPt[G,E]] {
    def mG = INT[G]
    def mE = INT[E]
    def tp = fltPtType[G,E]
  }
  abstract class FltPtOp2[G:INT,E:INT,R:Type] extends Op[R] {
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
    def mirror(f:Tx) = flt_convert[G,E,G2,E2](f(x))
  }
  case class FltPtToFixPt[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Exp[FltPt[G,E]]) extends FixPtOp[S,I,F] {
    def mirror(f:Tx) = flt_to_fix[G,E,S,I,F](f(x))
  }
  case class StringToFltPt[G:INT,E:INT](x: Exp[Text]) extends FltPtOp[G,E] {
    def mirror(f:Tx) = text_to_fltpt[G,E](x)
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
  def flt_to_fix[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Exp[FltPt[_,_]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = {
    stage(FltPtToFixPt[G,E,S,I,F](x.asInstanceOf[Exp[FltPt[G,E]]]))(ctx)
  }
  def text_to_fltpt[G:INT,E:INT](x: Exp[Text])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = x match {
    case Const(c: String) => string2fltpt[G,E](c).s
    case _ => stage(StringToFltPt[G,E](x))(ctx)
  }

  /** Other rewrite rules **/
  override def bool_not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Op(node@FltNeq(a,b)) => stage( FltEql(a,b)(node.mG,node.mE) )(ctx)
    case Op(node@FltEql(a,b)) => stage( FltNeq(a,b)(node.mG,node.mE) )(ctx)
    case Op( node@FltLt(a,b)) => stage( FltLeq(b,a)(node.mG,node.mE) )(ctx)
    case Op(node@FltLeq(a,b)) => stage(  FltLt(b,a)(node.mG,node.mE) )(ctx)
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