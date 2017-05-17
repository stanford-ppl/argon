package argon.lang

import argon._
import argon.nodes._
import argon.typeclasses._
import forge._

case class FltPt[G:INT,E:INT](s: Exp[FltPt[G,E]]) extends MetaAny[FltPt[G,E]] {
  override type Internal = BigDecimal
  protected val flt = FltPt
  @api def unary_-(): FltPt[G,E] = FltPt(flt.neg(this.s))
  @api def + (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.add(this.s,that.s))
  @api def - (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.sub(this.s,that.s))
  @api def * (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.mul(this.s,that.s))
  @api def / (that: FltPt[G,E]): FltPt[G,E] = FltPt(flt.div(this.s,that.s))
  @api def < (that: FltPt[G,E]): MBoolean   = MBoolean( flt.lt(this.s,that.s))
  @api def <=(that: FltPt[G,E]): MBoolean   = MBoolean(flt.leq(this.s,that.s))
  @api def > (that: FltPt[G,E]): MBoolean   = MBoolean( flt.lt(that.s,this.s))
  @api def >=(that: FltPt[G,E]): MBoolean   = MBoolean(flt.leq(that.s,this.s))
  @api def ===(that: FltPt[G,E]): MBoolean  = MBoolean(flt.eql(this.s,that.s))
  @api def =!=(that: FltPt[G,E]): MBoolean  = MBoolean(flt.neq(this.s,that.s))

  @api override def toText = String.ify(this)
}


object FltPt {
  /** Static methods **/
  @internal def wrap[G:INT,E:INT](s: Exp[FltPt[G,E]]): FltPt[G,E] = new FltPt[G,E](s)
  @api def apply[G:INT,E:INT](x: Int, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))
  @api def apply[G:INT,E:INT](x: Long, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))
  @api def apply[G:INT,E:INT](x: Float, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))
  @api def apply[G:INT,E:INT](x: Double, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))
  @api def apply[G:INT,E:INT](x: CString, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))
  @api def apply[G:INT,E:INT](x: BigInt, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))
  @api def apply[G:INT,E:INT](x: BigDecimal, force: CBoolean = false): FltPt[G,E] = FltPt.wrap[G,E](const(x, force))


  /** Constants **/
  @internal def literalToBigDecimal[G:INT,E:INT](x: Any, force: CBoolean): BigDecimal = {
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
      case x: CString => makeFloat(BigDecimal(x))
      case c =>
        error(s"$c cannot be lifted to a floating point value")
        sys.exit()
    }
  }
  @internal def const[G:INT,E:INT](x: Any, force: CBoolean = true): Const[FltPt[G,E]] = {
    constant(literalToBigDecimal[G,E](x, force))
  }

  @internal def string2fltpt[G:INT,E:INT](x: CString): FltPt[G,E] = FltPt(const[G,E](x))


  /** Constructors **/
  @internal def neg[G:INT,E:INT](x: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = x match {
    case Const(c) => const[G,E](-c)
    case Op(FltNeg(x)) => x
    case _ => stage(FltNeg(x))(ctx)
  }
  @internal def add[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a), Const(b)) => const[G,E](a + b)
    case _ => stage(FltAdd(x,y))(ctx)
  }
  @internal def sub[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a), Const(b)) => const[G,E](a - b)
    case _ => stage(FltSub(x,y))(ctx)
  }
  @internal def mul[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a), Const(b)) => const[G,E](a * b)
    case _ => stage(FltMul(x,y))(ctx)
  }
  @internal def div[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[FltPt[G,E]] = (x,y) match {
    case (Const(a), Const(b)) => const[G,E](a / b)
    case _ => stage(FltDiv(x,y))(ctx)
  }
  @internal def lt[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[MBoolean] = (x,y) match {
    case (Const(a), Const(b)) => MBoolean.const(a < b)
    case _ => stage(FltLt(x,y))(ctx)
  }
  @internal def leq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[MBoolean] = (x,y) match {
    case (Const(a), Const(b)) => MBoolean.const(a <= b)
    case _ => stage(FltLeq(x,y))(ctx)
  }
  @internal def neq[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[MBoolean] = (x,y) match {
    case (Const(a), Const(b)) => MBoolean.const(a != b)
    case _ => stage(FltNeq(x,y))(ctx)
  }
  @internal def eql[G:INT,E:INT](x: Exp[FltPt[G,E]], y: Exp[FltPt[G,E]]): Exp[MBoolean] = (x,y) match {
    case (Const(a), Const(b)) => MBoolean.const(a == b)
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
  @internal def from_string[G:INT,E:INT](x: Exp[MString]): Exp[FltPt[G,E]] = x match {
    case Const(c) => string2fltpt[G,E](c).s
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
  implicit def string2fltpt[G:INT,E:INT] = new Cast[MString,FltPt[G,E]] {
    @internal def apply(x: MString): FltPt[G,E] = FltPt(FltPt.from_string[G,E](x.s))
  }
}

trait FltPtApi {
  type Double = FltPt[_53,_11]
  type Float  = FltPt[_24, _8]
  type Half   = FltPt[_11, _5]
}


