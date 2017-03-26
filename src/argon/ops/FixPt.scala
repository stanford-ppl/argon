package argon.ops

import argon.core.Staging
import argon.typeclasses._
import org.virtualized.stageany

trait FixPtApi extends FixPtExp with BitsApi with NumApi with OrderApi with CastApi with BoolApi { this: TextApi =>
  type Long  = Int64
  type Int   = Int32
  type Short = Int16
  type Char  = Int8
}

@stageany
trait FixPtExp extends Staging with BitsExp with NumExp with OrderExp with CustomBitWidths with CastExp with BoolExp {
  this: TextExp =>

  /** Type Aliases **/
  type Int64 = FixPt[TRUE,_64,_0]
  type Int32 = FixPt[TRUE,_32,_0]
  type Int16 = FixPt[TRUE,_16,_0]
  type Int8  = FixPt[TRUE,_8,_0]

  type Index = Int32  // Addressing, sizes, etc.

  /** Infix Methods **/
  case class FixPt[S:BOOL,I:INT,F:INT](s: Exp[FixPt[S,I,F]]) extends StageAny[FixPt[S,I,F]] {
    def unary_-(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_neg(this.s))
    def unary_~(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_inv(this.s))
    def + (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_add(this.s,that.s))
    def - (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_sub(this.s,that.s))
    def * (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_mul(this.s,that.s))
    def / (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_div(this.s,that.s))
    def & (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(fix_and(this.s,that.s))
    def | (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt( fix_or(this.s,that.s))
    def < (that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = Bool( fix_lt(this.s,that.s))
    def <=(that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = Bool(fix_leq(this.s,that.s))
    def > (that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = Bool( fix_lt(that.s,this.s))
    def >=(that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = Bool(fix_leq(that.s,this.s))

    def ===(that: FixPt[S,I,F])(implicit ctx: SrcCtx) = Bool(fix_eql(this.s, that.s))
    def =!=(that: FixPt[S,I,F])(implicit ctx: SrcCtx) = Bool(fix_neq(this.s, that.s))

    override def toText(implicit ctx: SrcCtx) = textify(this)

  }

  implicit class FixPtIntLikeOps[S:BOOL,I:INT](x: FixPt[S,I,_0]) {
    def %(y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = mod(x, y)
  }

  /** Direct methods **/
  def mod[S:BOOL,I:INT](x: FixPt[S,I,_0], y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = FixPt[S,I,_0](fix_mod(x.s, y.s))


  /** Type classes **/
  // --- Staged
  class FixPtType[S,I,F](val mS: BOOL[S], val mI: INT[I], val mF: INT[F]) extends FStaged[FixPt[S,I,F]] {
    override def wrapped(s: Exp[FixPt[S,I,F]]): FixPt[S,I,F] = FixPt[S,I,F](s)(mS,mI,mF)
    override def typeArguments = Nil
    override def stagedClass = classOf[FixPt[S,I,F]]
    override def isPrimitive = true

    def isSigned: Boolean = mS.v
    def intBits: Int = mI.v
    def fracBits: Int = mF.v

    override def equals(x: Any) = x match {
      case that: FixPtType[_,_,_] => this.mS == that.mS && this.mI == that.mI && this.mF == that.mF
      case _ => false
    }
    override def hashCode() = (mS,mI,mF).##
  }
  implicit def fixPtType[S:BOOL,I:INT,F:INT]: Staged[FixPt[S,I,F]] = new FixPtType[S,I,F](BOOL[S],INT[I],INT[F])

  object FixPtType {
    def unapply(x :Staged[_]):Option[(Boolean, Int, Int)] = x match {
      case tp:FixPtType[_,_,_] => Some((tp.isSigned, tp.intBits, tp.fracBits))
      case _ => None
    }
  }
  object IntType extends FixPtType(BOOL[TRUE],INT[_32],INT[_0]) {
    def unapply(x: Staged[_]): Boolean = x match {
      case FixPtType(true, 32, 0) => true
      case _ => false
    }
  }
  object LongType extends FixPtType(BOOL[TRUE],INT[_64],INT[_0]) {
    def unapply(x: Staged[_]): Boolean = x match {
      case FixPtType(true, 64, 0) => true
      case _ => false
    }
  }

  def isFixPtType(x: Staged[_]) = FixPtType.unapply(x).isDefined
  def isInt32Type(x: Staged[_]) = IntType.unapply(x)

  // --- Num
  class FixPtNum[S:BOOL,I:INT,F:INT] extends Num[FixPt[S,I,F]] {
    override def negate(x: FixPt[S,I,F])(implicit ctx: SrcCtx) = -x
    override def plus(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x + y
    override def minus(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x - y
    override def times(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x * y
    override def divide(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x / y

    override def zero(implicit ctx: SrcCtx) = int2fixpt[S,I,F](0)
    override def one(implicit ctx: SrcCtx) = int2fixpt[S,I,F](1)
    override def random(max: Option[FixPt[S,I,F]])(implicit ctx: SrcCtx): FixPt[S, I, F] = FixPt[S, I, F](fix_random[S, I, F](max.map(_.s)))
    override def length: Int = INT[I].v + INT[F].v

    override def lessThan(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x < y
    override def lessThanOrEqual(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x <= y
    override def equal(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x === y
  }
  implicit def __fixPtNum[S:BOOL,I:INT,F:INT]: Num[FixPt[S,I,F]] = new FixPtNum[S,I,F]()

  override protected def bitsUnapply[T <: StageAny[T]](tp: Staged[T]): Option[Bits[T]] = tp match {
    case tp: FixPtType[_,_,_] => Some(new FixPtNum()(tp.mS,tp.mI,tp.mF).asInstanceOf[Bits[T]])
    case _ => super.bitsUnapply(tp)
  }

  // --- Lift
  implicit object Int2FixPt extends Lift[Int,Int32] {
    override def lift(x: Int)(implicit ctx: SrcCtx) = int2fixpt(x)
  }

  implicit object Long2FixPt extends Lift[Long,Int64] {
    override def lift(x: Long)(implicit ctx: SrcCtx) = long2fixpt(x)
  }


  /** Constant lifting **/
  private def literalToBigDecimal[S:BOOL,I:INT,F:INT](x: Any, enWarn: Boolean = true)(implicit ctx: SrcCtx): BigDecimal = {
    val sign = BOOL[S].v
    val ibits = INT[I].v
    val fbits = INT[F].v

    val tp = fixPtType[S,I,F]

    val MAX_INTEGRAL_VALUE = BigDecimal( if (sign) (BigInt(1) << (ibits-1)) - 1 else (BigInt(1) << ibits) - 1 )
    val MIN_INTEGRAL_VALUE = BigDecimal( if (sign) -(BigInt(1) << (ibits-1)) else BigInt(0) )

    def makeFixPt(v: BigDecimal): BigDecimal = {
      if (v > MAX_INTEGRAL_VALUE) {
        if (enWarn) new LiftOverflowError(tp, x)(ctx)
        MAX_INTEGRAL_VALUE
      }
      else if (v < MIN_INTEGRAL_VALUE) {
        if (enWarn) new LiftUnderflowError(tp, x)(ctx)
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

  private def createConstant[S:BOOL,I:INT,F:INT](x: Any, enWarn: Boolean = true)(implicit ctx: SrcCtx): Const[FixPt[S,I,F]] = {
    constant[FixPt[S,I,F]](literalToBigDecimal[S,I,F](x,enWarn))
  }
  def fixpt[S:BOOL,I:INT,F:INT](x: BigDecimal)(implicit ctx: SrcCtx): Const[FixPt[S,I,F]] = createConstant[S,I,F](x, enWarn=false)
  def int32(x: BigDecimal)(implicit ctx: SrcCtx): Const[Int32] = createConstant[TRUE,_32,_0](x, enWarn = true)


  implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int)(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(createConstant[S,I,F](x))
  implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long)(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(createConstant[S,I,F](x))
  def string2fixpt[S:BOOL,I:INT,F:INT](x: String)(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(createConstant[S,I,F](x))




  def intParam(c: Int)(implicit ctx: SrcCtx): Param[Int32] = parameter[Int32](literalToBigDecimal[TRUE,_32,_0](c))




  /** Casting **/
  override protected def cast[T: Staged:Num,R:StageAny:Num](x: T)(implicit ctx: SrcCtx): R = (ftyp[T],ftyp[R]) match {
    case (a:FixPtType[s,i,f],b:FixPtType[s2,i2,f2]) =>
      implicit val mS: BOOL[s] = a.mS
      implicit val mI: INT[i] = a.mI
      implicit val mF: INT[f] = a.mF
      implicit val mS2: BOOL[s2] = b.mS
      implicit val mI2: INT[i2] = b.mI
      implicit val mF2: INT[f2] = b.mF
      wrap(fix_convert[s,i,f,s2,i2,f2](x.asInstanceOf[FixPt[s,i,f]].s)).asInstanceOf[R]

    case _ => super.cast[T,R](x)
  }

  override protected def castLift[R:StageAny:Num](x: Any)(implicit ctx: SrcCtx): R = ftyp[R] match {
    case tp:FixPtType[s,i,f] =>
      implicit val mS: BOOL[s] = tp.mS
      implicit val mI: INT[i] = tp.mI
      implicit val mF: INT[f] = tp.mF
      FixPt(createConstant[s,i,f](x, enWarn = false)).asInstanceOf[R]
    case _ => super.castLift[R](x)
  }

  /** IR Nodes **/
  abstract class FixPtOp[S:BOOL,I:INT,F:INT] extends Op[FixPt[S,I,F]] {
    def mS = BOOL[S]
    def mI = INT[I]
    def mF = INT[F]
    def tp = fixPtType[S,I,F]
  }
  abstract class FixPtOp2[S:BOOL,I:INT,F:INT,R:StageAny] extends Op[R] {
    def mS = BOOL[S]
    def mI = INT[I]
    def mF = INT[F]
    def tp = fixPtType[S,I,F]
  }

  case class FixInv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_inv(f(x)) }
  case class FixNeg[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_neg(f(x)) }

  case class FixAdd[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_add(f(x), f(y)) }
  case class FixSub[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_sub(f(x), f(y)) }
  case class FixMul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_mul(f(x), f(y)) }
  case class FixDiv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_div(f(x), f(y)) }
  case class FixAnd[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_and(f(x), f(y)) }
  case class FixOr [S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) =  fix_or(f(x), f(y)) }
  case class FixLt [S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_lt(f(x), f(y)) }
  case class FixLeq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_leq(f(x), f(y)) }
  case class FixNeq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_neq(f(x), f(y)) }
  case class FixEql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_eql(f(x), f(y)) }
  case class FixMod[S:BOOL,I:INT](x: Exp[FixPt[S,I,_0]], y: Exp[FixPt[S,I,_0]]) extends FixPtOp[S,I,_0] { def mirror(f:Tx) = fix_mod(f(x), f(y)) }

  case class FixRandom[S:BOOL,I:INT,F:INT](max: Option[Exp[FixPt[S,I,F]]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_random[S,I,F](f(max)) }

  case class FixConvert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Exp[FixPt[S,I,F]]) extends FixPtOp[S2,I2,F2] {
    def mirror(f:Tx) = fix_convert[S,I,F,S2,I2,F2](f(x))
  }


  /** Constructors **/
  def fix_neg[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = x match {
    case Const(c:BigDecimal) => fixpt[S,I,F](-c)
    case Op(FixNeg(x)) => x
    case _ => stage(FixNeg(x))(ctx)
  }
  def fix_inv[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = x match {
    case Const(c:BigDecimal) if c.isWhole => fixpt[S,I,F](BigDecimal(~c.toBigInt))
    case Op(FixInv(x)) => x
    case _ => stage(FixInv(x))(ctx)
  }
  def fix_add[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => fixpt[S,I,F](a + b)
    case (a, Const(0)) => a                               // a + 0 => a
    case (Const(0), b) => b                               // 0 + a => a
    case (a, Op(FixNeg(b))) if a == b => fixpt[S,I,F](0)  // a + -a => 0
    case (Op(FixNeg(a)), b) if a == b => fixpt[S,I,F](0)  // -a + a => 0
    case (Op(FixSub(a,b)), c) if b == c => a              // a - b + b => a
    case (a, Op(FixSub(b,c))) if a == c => b              // a + (b - a) => b
    case _ => stage(FixAdd(x,y))(ctx)
  }
  def fix_sub[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigDecimal), Const(b:BigDecimal)) => fixpt[S,I,F](a - b)
    case (a, Const(0)) => a                                      // a - 0 => a
    case (Const(0), a) => stage(FixNeg(a))(ctx)                  // 0 - a => -a
    case (Op(FixAdd(a,b)), c) if a == c => b                     // a + b - a => b
    case (a, Op(FixAdd(b,c))) if a == c => stage(FixNeg(b))(ctx) // a - (b + a) => -b
    case (a, Op(FixAdd(b,c))) if a == b => stage(FixNeg(c))(ctx) // a - (a + b) => -b
    case _ => stage(FixSub(x,y))(ctx)
  }

  def fix_mul[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => fixpt[S,I,F](a * b)
    case (_, b@Const(0)) => b
    case (a@Const(0), _) => a
    case (a, Const(1)) => a
    case (Const(1), b) => b
    case _ => stage(FixMul(x, y) )(ctx)
  }
  def fix_div[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => fixpt[S,I,F](a / b)
    case (a, Const(1)) => a
    case (_, Const(0)) => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(FixDiv(x,y))(ctx)
  }
  def fix_and[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isWhole => fixpt[S,I,F](BigDecimal(a.toBigInt & b.toBigInt))
    case (a@Const(0), _) => a
    case (_, b@Const(0)) => b
    case _ => stage(FixAnd(x,y))(ctx)
  }
  def fix_or[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) if a.isWhole && b.isWhole => fixpt[S,I,F](BigDecimal(a.toBigInt | b.toBigInt))
    case (a, Const(0)) => a
    case (Const(0), b) => b
    case _ => stage(FixOr(x,y))(ctx)
  }
  def fix_lt[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => bool(a < b)
    case _ => stage( FixLt(x,y))(ctx)
  }

  def fix_leq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => bool(a <= b)
    case _ => stage(FixLeq(x,y))(ctx)
  }
  def fix_neq[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => bool(a != b)
    case _ => stage(FixNeq(x,y))(ctx)
  }
  def fix_eql[S:BOOL,I:INT,F:INT](x: Exp[FixPt[S,I,F]], y: Exp[FixPt[S,I,F]])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => bool(a == b)
    case _ => stage(FixEql(x,y))(ctx)
  }
  def fix_mod[S:BOOL,I:INT](x: Exp[FixPt[S,I,_0]], y: Exp[FixPt[S,I,_0]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,_0]] = (x,y) match {
    case (Const(a: BigDecimal), Const(b: BigDecimal)) => fixpt[S,I,_0](a % b)
    case (a, Const(1)) => fixpt[S,I,_0](0)
    case _ => stage(FixMod(x,y))(ctx)
  }
  def fix_random[S:BOOL,I:INT,F:INT](max: Option[Exp[FixPt[S,I,F]]])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = {
    stageSimple(FixRandom[S,I,F](max))(ctx)
  }

  def fix_convert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Exp[FixPt[_,_,_]])(implicit ctx: SrcCtx): Exp[FixPt[S2,I2,F2]] = {
    stage(FixConvert[S,I,F,S2,I2,F2](x.asInstanceOf[Exp[FixPt[S,I,F]]]))(ctx)
  }


  /** Rewrite rules **/
  override def bool_not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Def(node@FixNeq(a,b)) => stage( FixEql(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixEql(a,b)) => stage( FixNeq(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def( node@FixLt(a,b)) => stage( FixLeq(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case Def(node@FixLeq(a,b)) => stage(  FixLt(a,b)(node.mS,node.mI,node.mF) )(ctx)
    case _ => super.bool_not(x)
  }


  /** Internal methods **/
  override def readable(x: Any): String = x match {
    case FixPtType(sign,ibits,fbits) => s"FixPt[$sign,$ibits,$fbits]"
    case _ => super.readable(x)
  }

  override def userReadable(x: Any): String = x match {
    case IntType()  => "Int"
    case LongType() => "Long"
    case tp:FixPtType[_,_,_] => u"FixPt[${tp.mS},${tp.mI},${tp.mF}]"
    case _ => super.userReadable(x)
  }
}

