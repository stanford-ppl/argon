package argon.ops

trait FixPts extends Nums with Bools with CustomBitWidths {
  type FxP[T] <: Num[T]

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
    private def lift[S:BOOL,I:INT,F:INT]: FixPt[S,I,F] = int2fixpt[S,I,F](x)
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
    private def lift[S:BOOL,I:INT,F:INT]: FixPt[S,I,F] = long2fixpt[S,I,F](x)
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

  implicit def fixPtType[S:BOOL,I:INT,F:INT]: FxP[FixPt[S,I,F]]
  implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int): FixPt[S,I,F]
  implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long): FixPt[S,I,F]

  def mod[S:BOOL,I:INT](x: FixPt[S,I,_0], y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0]
}
trait FixPtApi extends FixPts with NumApi with BoolApi {
  type Long  = Int64
  type Int   = Int32
  type Short = Int16
  type Char  = Int8
}


trait FixPtExp extends FixPts with NumExp with BoolExp {
  /** API Wrapper **/
  case class FixPt[S:BOOL,I:INT,F:INT](s: Sym[FixPt[S,I,F]]) extends FixPtOps[S,I,F] {
    def unary_-(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixNeg(this.s))(ctx)
    def unary_~(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixInv(this.s))(ctx)
    def + (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixAdd(this.s,that.s))(ctx)
    def - (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixSub(this.s,that.s))(ctx)
    def * (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixMul(this.s,that.s))(ctx)
    def / (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixDiv(this.s,that.s))(ctx)
    def & (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = stage(FixAnd(this.s,that.s))(ctx)
    def | (that: FixPt[S,I,F])(implicit ctx: SrcCtx): FixPt[S,I,F] = stage( FixOr(this.s,that.s))(ctx)
    def < (that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = stage( FixLt(this.s,that.s))(ctx)
    def <=(that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = stage(FixLeq(this.s,that.s))(ctx)
    def > (that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = stage( FixLt(that.s,this.s))(ctx)
    def >=(that: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool         = stage(FixLeq(that.s,this.s))(ctx)
  }
  def infix_!=[S:BOOL,I:INT,F:INT](x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = stage(FixNeq(x.s,y.s))(ctx)
  def infix_==[S:BOOL,I:INT,F:INT](x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx): Bool = stage(FixEql(x.s,y.s))(ctx)

  /** Staged type **/
  abstract class FxP[T] extends Num[T] {
    def isSigned: Boolean
    def intBits: Int
    def fracBits: Int

    def mS: BOOL[_]
    def mI: INT[_]
    def mF: INT[_]
  }

  class FixPtType[S:BOOL,I:INT,F:INT]() extends FxP[FixPt[S,I,F]] {
    override def wrap(s: Sym[FixPt[S,I,F]]): FixPt[S,I,F] = FixPt[S,I,F](s)
    override def unwrap(x: FixPt[S,I,F]) = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[FixPt[S,I,F]]
    override def isPrimitive = true

    lazy val zero = const[FixPt[S,I,F]](0)
    lazy val one = const[FixPt[S,I,F]](1)
    override def random(implicit ctx: SrcCtx): FixPt[S, I, F] = randomFixPt[S,I,F]

    override def negate(x: FixPt[S,I,F])(implicit ctx: SrcCtx) = -x
    override def plus(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x + y
    override def minus(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x - y
    override def times(x: FixPt[S,I,F], y: FixPt[S,I,F])(implicit ctx: SrcCtx) = x * y

    def isSigned: Boolean = implicitly[BOOL[S]].v
    def intBits: Int = implicitly[INT[I]].v
    def fracBits: Int = implicitly[INT[F]].v
    override def mS = BOOL[S]
    override def mI = INT[I]
    override def mF = INT[F]

    override def hashCode() = (isSigned, intBits, fracBits).##
    override def equals(x: Any) = x match {
      case t:FxP[_] => t.isSigned == this.isSigned &&
                       t.intBits == this.intBits &&
                       t.fracBits == this.fracBits
      case _ => false
    }
  }
  implicit def fixPtType[S:BOOL,I:INT,F:INT]: FxP[FixPt[S,I,F]] = new FixPtType[S,I,F]

  object FixPtType {
    def unapply(x:Staged[_]):Option[(Boolean, Int, Int)] = x match {
      case tp:FixPtType[_, _, _] => Some((tp.isSigned, tp.intBits, tp.fracBits))
      case _ => None
    }
  }

  /** Constant lifting **/
  private def createConstant[S:BOOL,I:INT,F:INT](x: Any, enWarn: Boolean = true): FixPt[S,I,F] = {
    val sign = BOOL[S].v
    val ibits = INT[I].v
    val fbits = INT[F].v

    val s = if (sign) "signed" else "unsigned"
    val t = s"$ibits.$fbits"
    val tp = s"$s $t fixed point"
    val FXP = readable(fixPtType[S,I,F])

    val MAX_INTEGRAL_VALUE = if (sign) (BigInt(1) << (ibits-1)) - 1 else (BigInt(1) << ibits) - 1
    val MIN_INTEGRAL_VALUE = if (sign) -(BigInt(1) << (ibits-1)) else BigInt(0)

    def makeInteger(v: BigInt): FixPt[S,I,F] = {
      val clampedV = if (v > MAX_INTEGRAL_VALUE) {
        if (enWarn) {
          warn(s"Loss of precision detected: $tp cannot represent $x. Using maximum $MAX_INTEGRAL_VALUE.")
          warn(s"Use the explicit annotation $x.as[$FXP] to clear this warning")
        }
        MAX_INTEGRAL_VALUE
      }
      else if (v < MIN_INTEGRAL_VALUE) {
        if (enWarn) {
          warn(s"Loss of precision detected: $tp cannot represent $x. Using minimum $MIN_INTEGRAL_VALUE.")
          warn(s"Use the explicit annotation $x.as[$FXP] to clear this warning")
        }
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
  implicit def int2fixpt[S:BOOL,I:INT,F:INT](x: Int): FixPt[S,I,F] = createConstant[S,I,F](x)
  implicit def long2fixpt[S:BOOL,I:INT,F:INT](x: Long): FixPt[S,I,F] = createConstant[S,I,F](x)

  def fixpt[T](x: BigInt)(tp: FxP[T]): Sym[T] = {
    createConstant(x, enWarn=false)(tp.mS,tp.mI,tp.mF).asInstanceOf[Sym[T]]
  }



  /** Nodes **/
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

  case class FixInv[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = ~f(x) }
  case class FixNeg[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = -f(x) }

  case class FixAdd[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = f(x) + f(y) }
  case class FixSub[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = f(x) - f(y) }
  case class FixMul[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = f(x) * f(y) }
  case class FixDiv[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]])(implicit val ctx: SrcCtx) extends FixPtOp[S,I,F] {
    // TODO: Better spot for this?
    y match {
      case Const(0) => warn(ctx, "Division by constant 0 detected")
      case _ =>
    }

    def mirror(f:Tx) = f(x) / f(y)
  }
  case class FixAnd[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = f(x) & f(y) }
  case class FixOr [S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp[S,I,F] { def mirror(f:Tx) = f(x) | f(y) }
  case class FixLt [S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = f(x) < f(y) }
  case class FixLeq[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = f(x) <= f(y) }
  case class FixNeq[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = infix_!=(f(x),f(y)) }
  case class FixEql[S:BOOL,I:INT,F:INT](x: Sym[FixPt[S,I,F]], y: Sym[FixPt[S,I,F]]) extends FixPtOp2[S,I,F,Bool] { def mirror(f:Tx) = infix_==(f(x),f(y)) }
  case class FixMod[S:BOOL,I:INT](x: Sym[FixPt[S,I,_0]], y: Sym[FixPt[S,I,_0]]) extends FixPtOp[S,I,_0] { def mirror(f:Tx) = f(x) % f(y) }

  case class RandomFixPt[S:BOOL,I:INT,F:INT]() extends FixPtOp[S,I,F] { def mirror(f:Tx) = randomFixPt[S,I,F] }


  /** Direct methods **/
  def mod[S:BOOL,I:INT](x: FixPt[S,I,_0], y: FixPt[S,I,_0])(implicit ctx: SrcCtx): FixPt[S,I,_0] = stage(FixMod(x.s,y.s))(ctx)
  def randomFixPt[S:BOOL,I:INT,F:INT](implicit ctx: SrcCtx): FixPt[S,I,F] = stageSimple(RandomFixPt[S,I,F]())(ctx)


  /** Rewrite rules **/
  rewrite[FixInv[_,_,_]] {
    case node@FixInv(Const(c:BigInt)) => fixpt(~c)(node.tp)
    case Def(FixInv(x)) => x
  }
  rewrite[FixNeg[_,_,_]] {
    case node@FixNeg(Const(c:BigInt)) => fixpt(-c)(node.tp)
    case FixNeg(Def(FixNeg(x))) => x
  }
  rewrite[FixAdd[_,_,_]] {
    case node@FixAdd(Const(a: BigInt), Const(b: BigInt)) => fixpt(a + b)(node.tp)
    case FixAdd(a, Const(0)) => a   // Note that Const comparison only checks value equality, not type
    case FixAdd(Const(0), b) => b
    case node@FixAdd(a, Def(FixNeg(b))) if a == b => fixpt(0)(node.tp)
    case node@FixAdd(Def(FixNeg(a)), b) if a == b => fixpt(0)(node.tp)
  }
  rewrite[FixSub[_,_,_]] {
    case node@FixSub(Const(a: BigInt), Const(b: BigInt)) => fixpt(a - b)(node.tp)
    case FixSub(a, Const(0)) => a
    case e@FixSub(Const(0), a) => stage(FixNeg(a)(e.mS,e.mI,e.mF))(here)(e.tp).s
  }
  rewrite[FixMul[_,_,_]] {
    case node@FixMul(Const(a: BigInt), Const(b: BigInt)) => fixpt(a * b)(node.tp)
    case FixMul(_, b@Const(0)) => b
    case FixMul(a@Const(0), _) => a
    case FixMul(a, Const(1)) => a
    case FixMul(Const(1), b) => b
  }
  rewrite[FixDiv[_,_,_]] {
    case node@FixDiv(Const(a: BigInt), Const(b: BigInt)) => fixpt(a / b)(node.tp)
    case FixDiv(a, Const(1)) => a
  }

  rewrite[FixAnd[_,_,_]] {
    case node@FixAnd(Const(a: BigInt), Const(b: BigInt)) => fixpt(a & b)(node.tp)
    case FixAnd(a@Const(0), _) => a
    case FixAnd(_, b@Const(0)) => b
  }

  rewrite[FixOr[_,_,_]] {
    case node@FixOr(Const(a: BigInt), Const(b: BigInt)) => fixpt(a | b)(node.tp)
    case FixOr(a, Const(0)) => a
    case FixOr(Const(0), b) => b
  }

  rewrite[FixLt[_,_,_]] {
    case FixLt(Const(a: BigInt), Const(b: BigInt)) => bool(a < b)
  }
  rewrite[FixLeq[_,_,_]] {
    case FixLeq(Const(a: BigInt), Const(b: BigInt)) => bool(a <= b)
  }
  rewrite[FixNeq[_,_,_]] {
    case FixNeq(Const(a: BigInt), Const(b: BigInt)) => bool(a != b)
  }
  rewrite[FixEql[_,_,_]] {
    case FixEql(Const(a: BigInt), Const(b: BigInt)) => bool(a == b)
  }

  rewrite[FixMod[_,_]] {
    case node@FixMod(Const(a: BigInt), Const(b: BigInt)) => fixpt(a % b)(node.tp)
    case node@FixMod(a, Const(1)) => fixpt(0)(node.tp)
  }

  rewrite[Not] {
    case Not(Def(node@FixNeq(x,y))) => stage( FixEql(x,y)(node.mS,node.mI,node.mF) )(here).s
    case Not(Def(node@FixEql(x,y))) => stage( FixNeq(x,y)(node.mS,node.mI,node.mF) )(here).s
    case Not(Def(node@FixLt(x,y)))  => stage( FixLeq(y,x)(node.mS,node.mI,node.mF) )(here).s
    case Not(Def(node@FixLeq(x,y))) => stage( FixLt(y,x)(node.mS,node.mI,node.mF) )(here).s
  }

  override def readable(x: Any): String = x match {
    case FixPtType(true,64,0) => s"Long"
    case FixPtType(true,32,0) => s"Int"
    case FixPtType(true,16,0) => s"Short"
    case FixPtType(true,8,0)  => s"Char"
    case FixPtType(sign,ibits,fbits) => s"FixPt[$sign,$ibits,$fbits]"
    case _ => super.readable(x)
  }
}



