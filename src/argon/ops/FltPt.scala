package argon.ops

trait FltPts extends Nums with Bools with CustomBitWidths {
  type FlP[T] <: Num[T]
  type FltPt[G,E] <: FltPtOps[G,E]
  // Significand bits includes sign bit
  // ASSUMPTION: floating point representation is always signed (for now)
  type Float16 = FltPt[_11,_5]
  type Float32 = FltPt[_24,_8]
  type Float64 = FltPt[_53,_11]

  protected trait FltPtOps[G,E] {
    def unary_-(implicit ctx: SrcCtx): FltPt[G,E]
    def + (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E]
    def - (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E]
    def * (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E]
    def / (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E]
    def < (that: FltPt[G,E])(implicit ctx: SrcCtx): Bool
    def <=(that: FltPt[G,E])(implicit ctx: SrcCtx): Bool
    def > (that: FltPt[G,E])(implicit ctx: SrcCtx): Bool
    def >=(that: FltPt[G,E])(implicit ctx: SrcCtx): Bool
  }

  implicit class IntFltPtOps(x: Int) {
    private def lift[G:INT,E:INT]: FltPt[G,E] = int2fltpt[G,E](x)
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
    private def lift[G:INT,E:INT]: FltPt[G,E] = long2fltpt[G,E](x)
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
    private def lift[G:INT,E:INT]: FltPt[G,E] = float2fltpt[G,E](x)
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
    private def lift[G:INT,E:INT]: FltPt[G,E] = double2fltpt[G,E](x)
    def + [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] + y
    def - [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] - y
    def * [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] * y
    def / [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = lift[G,E] / y
    def < [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] < y
    def <=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] <= y
    def > [G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] > y
    def >=[G:INT,E:INT](y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = lift[G,E] >= y
  }


  implicit def fltPtType[G:INT,E:INT]: FlP[FltPt[G,E]]
  implicit def int2fltpt[G:INT,E:INT](x: Int): FltPt[G,E]
  implicit def long2fltpt[G:INT,E:INT](x: Long): FltPt[G,E]
  implicit def float2fltpt[G:INT,E:INT](x: Float): FltPt[G,E]
  implicit def double2fltpt[G:INT,E:INT](x: Double): FltPt[G,E]
}
trait FltPtAPI extends FltPts with NumApi with BoolApi {
  type Double = Float64
  type Float = Float32
  type Half = Float16
}


trait FltPtExp extends FltPts with NumExp with BoolExp {
  /** API Wrapper **/
  case class FltPt[G:INT,E:INT](s: Sym[FltPt[G,E]]) extends FltPtOps[G,E] {
    def unary_-(implicit ctx: SrcCtx): FltPt[G,E] = stage(FltNeg(this.s))(ctx)
    def + (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = stage(FltAdd(this.s,that.s))(ctx)
    def - (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = stage(FltSub(this.s,that.s))(ctx)
    def * (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = stage(FltMul(this.s,that.s))(ctx)
    def / (that: FltPt[G,E])(implicit ctx: SrcCtx): FltPt[G,E] = stage(FltDiv(this.s,that.s))(ctx)
    def < (that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = stage( FltLt(this.s,that.s))(ctx)
    def <=(that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = stage(FltLeq(this.s,that.s))(ctx)
    def > (that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = stage( FltLt(that.s,this.s))(ctx)
    def >=(that: FltPt[G,E])(implicit ctx: SrcCtx): Bool       = stage(FltLeq(that.s,this.s))(ctx)
  }
  def infix_!=[G:INT,E:INT](x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = stage(FltNeq(x.s,y.s))(ctx)
  def infix_==[G:INT,E:INT](x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx): Bool = stage(FltEql(x.s,y.s))(ctx)


  /** Staged type **/
  abstract class FlP[T] extends Num[T] {
    def sigBits: Int
    def expBits: Int
    def mG: INT[_]
    def mE: INT[_]
  }

  class FltPtType[G:INT,E:INT]() extends FlP[FltPt[G,E]] {
    override def wrap(s: Sym[FltPt[G,E]]): FltPt[G,E] = FltPt[G,E](s)
    override def unwrap(x: FltPt[G,E]) = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[FltPt[G,E]]
    override def isPrimitive = true

    lazy val zero = const[FltPt[G,E]](0)
    lazy val one = const[FltPt[G,E]](1)
    override def random(implicit ctx: SrcCtx): FltPt[G,E] = randomFltPt[G,E]

    override def negate(x: FltPt[G,E])(implicit ctx: SrcCtx) = -x
    override def plus(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x + y
    override def minus(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x - y
    override def times(x: FltPt[G,E], y: FltPt[G,E])(implicit ctx: SrcCtx) = x * y

    def sigBits: Int = implicitly[INT[G]].v
    def expBits: Int = implicitly[INT[E]].v
    def mG: INT[_] = INT[G]
    def mE: INT[_] = INT[E]

    override def hashCode() = (sigBits, expBits).##
    override def equals(x: Any) = x match {
      case t:FlP[_] => t.sigBits == this.sigBits && t.expBits == this.expBits
      case _ => false
    }
  }
  implicit def fltPtType[G:INT,E:INT]: FlP[FltPt[G,E]] = new FltPtType[G,E]

  object FltPtType {
    def unapply(x:Staged[_]):Option[(Int, Int)] = x match {
      case tp:FltPtType[_, _] => Some((tp.sigBits, tp.expBits))
      case _ => None
    }
  }


  /** Constant lifting **/
  private def createConstant[G:INT,E:INT](x: Any, enWarn: Boolean = true): FltPt[G,E] = {
    val gbits = INT[G].v
    val ebits = INT[E].v
    val tp = s"$gbits.$ebits floating point"
    val FLP = readable(fltPtType[G,E])

    def makeFloat(v: BigDecimal): FltPt[G,E] = {
      // TODO: Precision checking
      const[FltPt[G,E]](v)
    }

    x match {
      case x: BigDecimal => makeFloat(x)
      case x: Int => makeFloat(BigDecimal(x))
      case x: Long => makeFloat(BigDecimal(x))
      case x: Float => makeFloat(BigDecimal(x))
      case x: Double => makeFloat(BigDecimal(x))
      case x: String => makeFloat(BigDecimal(x))
      case c =>
        error(s"$c cannot be lifted to a floating point value")
        sys.exit()
    }
  }

  implicit def int2fltpt[G:INT,E:INT](x: Int): FltPt[G,E] = createConstant[G,E](x)
  implicit def long2fltpt[G:INT,E:INT](x: Long): FltPt[G,E] = createConstant[G,E](x)
  implicit def float2fltpt[G:INT,E:INT](x: Float): FltPt[G,E] = createConstant[G,E](x)
  implicit def double2fltpt[G:INT,E:INT](x: Double): FltPt[G,E] = createConstant[G,E](x)

  def fltpt[T](x: BigDecimal)(tp: FlP[T]): Sym[T] = {
    createConstant(x, enWarn=false)(tp.mG,tp.mE).asInstanceOf[Sym[T]]
  }


  /** Nodes **/
  abstract class FltPtOp[G:INT,E:INT] extends Op[FltPt[G,E]] {
    def mG = INT[G]
    def mE = INT[E]
    def tp = fltPtType[G,E]
  }
  abstract class FltPtOp2[G:INT,E:INT,R:Staged] extends Op[R] {
    def mG = INT[G]
    def mE = INT[E]
    def tp = fltPtType[G,E]
  }

  case class FltNeg[G:INT,E:INT](x: Sym[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = -f(x) }

  case class FltAdd[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = f(x) + f(y) }
  case class FltSub[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = f(x) - f(y) }
  case class FltMul[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp[G,E] { def mirror(f:Tx) = f(x) * f(y) }
  case class FltDiv[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]])(implicit val ctx: SrcCtx) extends FltPtOp[G,E] {
    def mirror(f:Tx) = f(x) / f(y)
  }
  case class FltLt [G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = f(x) < f(y) }
  case class FltLeq[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = f(x) <= f(y) }
  case class FltNeq[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = infix_!=(f(x),f(y)) }
  case class FltEql[G:INT,E:INT](x: Sym[FltPt[G,E]], y: Sym[FltPt[G,E]]) extends FltPtOp2[G,E,Bool] { def mirror(f:Tx) = infix_==(f(x),f(y)) }

  case class RandomFltPt[G:INT,E:INT]() extends FltPtOp[G,E] { def mirror(f:Tx) = randomFltPt[G,E] }


  /** Rewrite rules **/
  rewrite[FltNeg[_,_]]{
    case node@FltNeg(Const(c:BigDecimal)) => fltpt(-c)(node.tp)
    case FltNeg(Def(FltNeg(x))) => x
  }
  rewrite[FltAdd[_,_]]{
    case node@FltAdd(Const(a:BigDecimal),Const(b:BigDecimal)) => fltpt(a + b)(node.tp)
  }
  rewrite[FltSub[_,_]]{
    case node@FltSub(Const(a:BigDecimal),Const(b:BigDecimal)) => fltpt(a - b)(node.tp)
  }
  rewrite[FltMul[_,_]]{
    case node@FltMul(Const(a:BigDecimal),Const(b:BigDecimal)) => fltpt(a * b)(node.tp)
  }
  rewrite[FltDiv[_,_]]{
    case node@FltDiv(Const(a:BigDecimal),Const(b:BigDecimal)) => fltpt(a / b)(node.tp)
  }
  rewrite[FltLt[_,_]]{
    case FltLt(Const(a:BigDecimal),Const(b:BigDecimal)) => bool(a < b)
  }
  rewrite[FltLeq[_,_]]{
    case FltLeq(Const(a:BigDecimal),Const(b:BigDecimal)) => bool(a <= b)
  }
  rewrite[FltNeq[_,_]]{
    case FltNeq(Const(a:BigDecimal),Const(b:BigDecimal)) => bool(a != b)
  }
  rewrite[FltEql[_,_]]{
    case FltEql(Const(a:BigDecimal),Const(b:BigDecimal)) => bool(a == b)
  }

  rewrite[Not] {
    case Not(Def(node@FltNeq(x,y))) => stage( FltEql(x,y)(node.mG,node.mE) )(here).s
    case Not(Def(node@FltEql(x,y))) => stage( FltNeq(x,y)(node.mG,node.mE) )(here).s
    case Not(Def(node@FltLt(x,y)))  => stage( FltLeq(y,x)(node.mG,node.mE) )(here).s
    case Not(Def(node@FltLeq(x,y))) => stage( FltLt(y,x)(node.mG,node.mE) )(here).s
  }

  /** Internal methods **/
  def randomFltPt[G:INT,E:INT](implicit ctx: SrcCtx): FltPt[G,E] = stageSimple(RandomFltPt[G,E]())(ctx)

  override def readable(x: Any): String = x match {
    case FltPtType(53,11) => "Double"
    case FltPtType(24,8)  => "Float"
    case FltPtType(11,5)  => "Half"
    case FltPtType(gbits,ebits) => s"FltPt[$gbits,$ebits]"
    case _ => super.readable(x)
  }
}