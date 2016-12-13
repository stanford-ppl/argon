package argon.ops

trait FixPts extends Nums with CustomBitWidths with Casts {
  type FixPt[S,I,F] <: FixPtOps[S,I,F]
  type Int64 = FixPt[TRUE,_64,_0]
  type Int32 = FixPt[TRUE,_32,_0]
  type Int16 = FixPt[TRUE,_16,_0]
  type Int8  = FixPt[TRUE,_8,_0]

  protected trait FixPtOps[S,I,F] {
    def unary_-(implicit ctx: SrcCtx): FixPt[S,I,F]
    def unary_~(implicit ctx: SrcCtx): FixPt[S,I,F]
    def + (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F]
    def - (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F]
    def * (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F]
    def / (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F]
    def & (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F]
    def | (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F]
    def < (that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool
    def <=(that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool
    def > (that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool
    def >=(that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool
  }
  implicit class FixPtIntLikeOps[S:BOOL,I:INT](x: FixPt[S,I,_0]) {
    def %(y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = mod(x, y)
  }

  implicit class IntFixPtOps(x: Int) {
    private def lift[S:BOOL,I:INT,F:INT](implicit ctx: SrcCtx): FixPt[S,I,F] = int2fixpt[S,I,F](x)
    def + [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] + y
    def - [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] - y
    def * [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] * y
    def / [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] / y
    def & [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] & y
    def | [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] | y
    def < [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] < y
    def <=[S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] <= y
    def > [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] > y
    def >=[S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] >= y
    def % [S:BOOL,I:INT](y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = lift[S,I,_0] % y
  }
  implicit class LongFixPtOps(x: Long) {
    private def lift[S:BOOL,I:INT,F:INT](implicit ctx: SrcCtx): FixPt[S,I,F] = long2fixpt[S,I,F](x)
    def + [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] + y
    def - [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] - y
    def * [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] * y
    def / [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] / y
    def & [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] & y
    def | [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = lift[S,I,F] | y
    def < [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] < y
    def <=[S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] <= y
    def > [S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] > y
    def >=[S:BOOL,I:INT,F:INT](y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = lift[S,I,F] >= y
    def % [S:BOOL,I:INT](y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = lift[S,I,_0] % y
  }

  implicit object Int2FixPt extends Lift[Int,Int32] { val staged = fixPtType[TRUE,_32,_0] }
  implicit object Long2FixPt extends Lift[Long,Int64] { val staged = fixPtType[TRUE,_64,_0] }

  implicit def fixPtType[S:BOOL,I:INT,F:INT]: Num[FixPt[S,I,F]]
  implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int)(implicit ctx: SrcCtx): FixPt[S,I,F]
  implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long)(implicit ctx: SrcCtx): FixPt[S,I,F]

  def mod[S:BOOL,I:INT](x: FixPt[S,I,_0], y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0]
}
trait FixPtApi extends FixPts with NumApi with CastApi {
  type Long  = Int64
  type Int   = Int32
  type Short = Int16
  type Char  = Int8
}


trait FixPtExp extends FixPts with NumExp with CastExp {
  /** API **/
  case class FixPt[S:BOOL,I:INT,F:INT](s: Sym[FixPt[S,I,F]]) extends FixPtOps[S,I,F] {
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
  }
  def infix_!=[S:BOOL,I:INT,F:INT](x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = Bool(fix_neq(x.s,y.s))
  def infix_==[S:BOOL,I:INT,F:INT](x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = Bool(fix_eql(x.s,y.s))

  def mod[S:BOOL,I:INT](x: FixPt[S,I,_0], y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = FixPt[S,I,_0](fix_mod(x.s, y.s))


  /** Staged Types **/
  class FixPtType[S:BOOL,I:INT,F:INT]() extends Num[FixPt[S,I,F]] {
    override def wrap(s: Sym[FixPt[S,I,F]]): FixPt[S,I,F] = FixPt[S,I,F](s)
    override def unwrap(x: FixPt[S,I,F]) = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[FixPt[S,I,F]]
    override def isPrimitive = true

    override def zero(implicit ctx: SrcCtx) = int2fixpt[S,I,F](0)
    override def one(implicit ctx: SrcCtx) = int2fixpt[S,I,F](1)
    override def random(implicit ctx: SrcCtx): FixPt[S, I, F] = FixPt[S,I,F](fix_random[S,I,F]())

    override def negate(x: FixPt[S,I,F])(implicit ctx: SrcCtx) = -x
    override def plus(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x + y
    override def minus(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x - y
    override def times(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x * y

    override def lessThan(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x < y
    override def equal(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = infix_==(x, y)

    def isSigned: Boolean = implicitly[BOOL[S]].v
    def intBits: Int = implicitly[INT[I]].v
    def fracBits: Int = implicitly[INT[F]].v
    def mS = BOOL[S]
    def mI = INT[I]
    def mF = INT[F]

    override def hashCode() = (isSigned, intBits, fracBits).##
    override def equals(x: Any) = x match {
      case t:FixPtType[_,_,_] => t.isSigned == this.isSigned &&
                                 t.intBits == this.intBits &&
                                 t.fracBits == this.fracBits
      case _ => false
    }
  }
  implicit def fixPtType[S:BOOL,I:INT,F:INT]: Num[FixPt[S,I,F]] = new FixPtType[S,I,F]

  object FixPtType {
    def unapply(x:Staged[_]):Option[(Boolean, Int, Int)] = x match {
      case tp:FixPtType[_, _, _] => Some((tp.isSigned, tp.intBits, tp.fracBits))
      case _ => None
    }
  }

  /** Constant Lifting **/
  private def createConstant[S:BOOL,I:INT,F:INT](x: Any, enWarn: Boolean = true)(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = {
    val sign = BOOL[S].v
    val ibits = INT[I].v
    val fbits = INT[F].v

    val tp = fixPtType[S,I,F]

    val MAX_INTEGRAL_VALUE = if (sign) (BigInt(1) << (ibits-1)) - 1 else (BigInt(1) << ibits) - 1
    val MIN_INTEGRAL_VALUE = if (sign) -(BigInt(1) << (ibits-1)) else BigInt(0)

    def makeInteger(v: BigInt): Sym[FixPt[S,I,F]] = {
      val clampedV = if (v > MAX_INTEGRAL_VALUE) {
        if (enWarn) new LiftOverflowError(tp, x)(ctx)
        MAX_INTEGRAL_VALUE
      }
      else if (v < MIN_INTEGRAL_VALUE) {
        if (enWarn) new LiftUnderflowError(tp, x)(ctx)
        MIN_INTEGRAL_VALUE
      }
      else v
      const[FixPt[S,I,F]](clampedV)
    }

    x match {
      case x: BigInt => makeInteger(x)
      case x: Int => makeInteger(BigInt(x))
      case x: Long => makeInteger(BigInt(x))
      case x: String if !x.exists(_ == '.') => makeInteger(BigInt(x))
      case c =>
        error(s"$c cannot be lifted to a fixed point value")
        sys.exit()
    }
  }
  implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int)(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(createConstant[S,I,F](x))
  implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long)(implicit ctx: SrcCtx): FixPt[S,I,F] = FixPt(createConstant[S,I,F](x))
  override def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l match {
    case Int2FixPt => FixPt(createConstant[TRUE,_32,_0](x)).asInstanceOf[B]
    case Long2FixPt => FixPt(createConstant[TRUE,_32,_0](x)).asInstanceOf[B]
    case _ => super.__lift(x)
  }

  def fixpt[S:BOOL,I:INT,F:INT](x: BigInt)(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = createConstant[S,I,F](x, enWarn=false)

  override protected def cast[T:Num,R:Num](x: T)(implicit ctx: SrcCtx): R = (num[T],num[R]) match {
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

  override protected def castLift[R:Num](x: Any)(implicit ctx: SrcCtx): R = num[R] match {
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
  abstract class FixPtOp2[S:BOOL,I:INT,F:INT,R:Staged] extends Op[R] {
    def mS = BOOL[S]
    def mI = INT[I]
    def mF = INT[F]
    def tp = fixPtType[S,I,F]
  }

  case class FixInv[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_inv(f(x)) }
  case class FixNeg[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_neg(f(x)) }

  case class FixAdd[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_add(f(x), f(y)) }
  case class FixSub[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_sub(f(x), f(y)) }
  case class FixMul[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_mul(f(x), f(y)) }
  case class FixDiv[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_div(f(x), f(y)) }
  case class FixAnd[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_and(f(x), f(y)) }
  case class FixOr [S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) =  fix_or(f(x), f(y)) }
  case class FixLt [S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_lt(f(x), f(y)) }
  case class FixLeq[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_leq(f(x), f(y)) }
  case class FixNeq[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_neq(f(x), f(y)) }
  case class FixEql[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = fix_eql(f(x), f(y)) }
  case class FixMod[S:BOOL,I:INT](x: Sym[FixPt[S,I,_0]], y: Sym[FixPt[S,I,_0]]) extends FixPtOp[S,I,_0] { def mirror(f:Tx) = fix_mod(f(x), f(y)) }

  case class RandomFixPt[S:BOOL,I:INT,F:INT]() extends FixPtOp[S,I,F] { def mirror(f:Tx) = fix_random[S,I,F]() }

  case class FixConvert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Sym[FixPt[S,I,F]]) extends FixPtOp[S2,I2,F2] {
    def mirror(f:Tx) = fix_convert[S,I,F,S2,I2,F2](x)
  }


  /** Smart constructors **/
  def fix_neg[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = x match {
    case Const(c:BigInt) => fixpt[S,I,F](-c)
    case Op(FixNeg(x)) => x
    case _ => stage(FixNeg(x))(ctx)
  }
  def fix_inv[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = x match {
    case Const(c:BigInt) => fixpt[S,I,F](~c)
    case Op(FixInv(x)) => x
    case _ => stage(FixInv(x))(ctx)
  }
  def fix_add[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigInt), Const(b:BigInt)) => fixpt[S,I,F](a + b)
    case (a, Const(0)) => a                               // a + 0 => a
    case (Const(0), b) => b                               // 0 + a => a
    case (a, Op(FixNeg(b))) if a == b => fixpt[S,I,F](0)  // a + -a => 0
    case (Op(FixNeg(a)), b) if a == b => fixpt[S,I,F](0)  // -a + a => 0
    case _ => stage(FixAdd(x,y))(ctx)
  }
  def fix_sub[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = (x,y) match {
    case (Const(a:BigInt), Const(b:BigInt)) => fixpt[S,I,F](a - b)
    case (a, Const(0)) => a
    case (Const(0), a) => stage(FixNeg(a))(ctx)
    case _ => stage(FixSub(x,y))(ctx)
  }

  def fix_mul[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => fixpt[S,I,F](a * b)
    case (_, b@Const(0)) => b
    case (a@Const(0), _) => a
    case (a, Const(1)) => a
    case (Const(1), b) => b
    case _ => stage(FixMul(x, y) )(ctx)
  }
  def fix_div[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => fixpt[S,I,F](a / b)
    case (a, Const(1)) => a
    case (_, Const(0)) => warn(ctx, "Division by constant 0 detected"); stage(FixDiv(x,y))(ctx)
    case _ => stage(FixDiv(x,y))(ctx)
  }
  def fix_and[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => fixpt[S,I,F](a & b)
    case (a@Const(0), _) => a
    case (_, b@Const(0)) => b
    case _ => stage(FixAnd(x,y))(ctx)
  }
  def fix_or[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => fixpt[S,I,F](a | b)
    case (a, Const(0)) => a
    case (Const(0), b) => b
    case _ => stage(FixOr(x,y))(ctx)
  }
  def fix_lt[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[Bool] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => bool(a < b)
    case _ => stage( FixLt(x,y))(ctx)
  }

  def fix_leq[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[Bool] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => bool(a <= b)
    case _ => stage(FixLeq(x,y))(ctx)
  }
  def fix_neq[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[Bool] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => bool(a != b)
    case _ => stage(FixNeq(x,y))(ctx)
  }
  def fix_eql[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit ctx: SrcCtx): Sym[Bool] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => bool(a == b)
    case _ => stage(FixEql(x,y))(ctx)
  }
  def fix_mod[S:BOOL,I:INT](x: Sym[FixPt[S,I,_0]], y: Sym[FixPt[S,I,_0]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,_0]] = (x,y) match {
    case (Const(a: BigInt), Const(b: BigInt)) => fixpt[S,I,_0](a % b)
    case (a, Const(1)) => fixpt[S,I,_0](0)
    case _ => stage(FixMod(x,y))(ctx)
  }
  def fix_random[S:BOOL,I:INT,F:INT]()(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = stageSimple(RandomFixPt[S,I,F]())(ctx)

  def fix_convert[S:BOOL,I:INT,F:INT,S2:BOOL,I2:INT,F2:INT](x: Sym[FixPt[_,_,_]])(implicit ctx: SrcCtx): Sym[FixPt[S2,I2,F2]] = {
    stage(FixConvert[S,I,F,S2,I2,F2](x.asInstanceOf[Sym[FixPt[S,I,F]]]))(ctx)
  }


  /** Rewrite rules **/
  override def bool_not(x: Sym[Bool])(implicit ctx: SrcCtx): Sym[Bool] = x match {
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
    case FixPtType(true,32,0) => "Int"
    case FixPtType(true,64,0) => "Long"
    case tp:FixPtType[_,_,_] => u"FixPt[${tp.mS},${tp.mI},${tp.mF}]"
    case _ => super.userReadable(x)
  }
}



