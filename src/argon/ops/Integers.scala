package argon.ops

import scala.language.implicitConversions

trait IntegerOps extends FixedPointAPI {
  type Int32 <: Int32Ops
  protected trait Int32Ops {
    def unary_-(implicit ctx: SrcCtx): Int32
    def unary_~(implicit ctx: SrcCtx): Int32
    def + (that: Int32)(implicit ctx: SrcCtx): Int32
    def - (that: Int32)(implicit ctx: SrcCtx): Int32
    def * (that: Int32)(implicit ctx: SrcCtx): Int32
    def / (that: Int32)(implicit ctx: SrcCtx): Int32
    def & (that: Int32)(implicit ctx: SrcCtx): Int32
    def | (that: Int32)(implicit ctx: SrcCtx): Int32
    def < (that: Int32)(implicit ctx: SrcCtx): Bool
    def <=(that: Int32)(implicit ctx: SrcCtx): Bool
    def > (that: Int32)(implicit ctx: SrcCtx): Bool
    def >=(that: Int32)(implicit ctx: SrcCtx): Bool
    def % (that: Int32)(implicit ctx: SrcCtx): Int32
  }

  def randomInt()(implicit ctx: SrcCtx): Int32

  implicit def lift(x: Int): Int32
  implicit val Int32Type: Num[Int32]
}
trait IntegerAPI extends IntegerOps {
  type Int = Int32
}

trait IntegerCore extends IntegerOps with FixedPointCore {
  /** Staged types **/
  case class Int32() extends FixedPoint[Signed,B32,B0] with Int32Ops { self =>
    override type LibType = Int
    override val tp = Int32Type.asInstanceOf[Typ[self.type]]

    def unary_-(implicit ctx: SrcCtx): Int32 = neg[Signed,B32,B0,Int32](this)
    def unary_~(implicit ctx: SrcCtx): Int32 = inv[Signed,B32,B0,Int32](this)
    def + (that: Int32)(implicit ctx: SrcCtx): Int32 = add[Signed,B32,B0,Int32](this,that)
    def - (that: Int32)(implicit ctx: SrcCtx): Int32 = sub[Signed,B32,B0,Int32](this,that)
    def * (that: Int32)(implicit ctx: SrcCtx): Int32 = mul[Signed,B32,B0,Int32](this,that)
    def / (that: Int32)(implicit ctx: SrcCtx): Int32 = div[Signed,B32,B0,Int32](this,that)
    def & (that: Int32)(implicit ctx: SrcCtx): Int32 = and[Signed,B32,B0,Int32](this,that)
    def | (that: Int32)(implicit ctx: SrcCtx): Int32 =  or[Signed,B32,B0,Int32](this,that)
    def < (that: Int32)(implicit ctx: SrcCtx): Bool  =  lt[Signed,B32,B0,Int32](this,that)
    def <=(that: Int32)(implicit ctx: SrcCtx): Bool  = leq[Signed,B32,B0,Int32](this,that)
    def > (that: Int32)(implicit ctx: SrcCtx): Bool  =  lt[Signed,B32,B0,Int32](that,this)
    def >=(that: Int32)(implicit ctx: SrcCtx): Bool  = leq[Signed,B32,B0,Int32](that,this)
    def % (that: Int32)(implicit ctx: SrcCtx): Int32 = mod[Signed,B32,Int32](this,that)
  }
  implicit object Int32Type extends FxPTyp[Int32] {
    override def next = Int32()
    override def typeArguments = Nil
    override def stagedClass = classOf[Int32]
    override def isPrimitive = true

    override def isSigned: Boolean = true
    override def intBits: Int = 32
    override def fracBits: Int = 0
    lazy val zero: Int32 = fresh[Int32].asConst(0)
    lazy val one: Int32  = fresh[Int32].asConst(1)
    def random(implicit ctx: SrcCtx): Int32 = randomInt()
  }

  /** Virtualized methods **/
  def infix_==(a: Int32, b: Int32)(implicit ctx: SrcCtx): Bool = eql[Signed,B32,B0,Int32](a,b)
  def infix_!=(a: Int32, b: Int32)(implicit ctx: SrcCtx): Bool = neq[Signed,B32,B0,Int32](a,b)
  def infix_equals(a: Int32, b: Int32)(implicit ctx: SrcCtx): Bool = eql[Signed,B32,B0,Int32](a,b)


  def isInt32[T:Typ] = typ[T] <:< Int32Type
  implicit def lift(c: Int): Int32 = fresh[Int32].asConst(c)

  def randomInt()(implicit ctx: SrcCtx): Int32 = stageSimple(RandomInt())(ctx)

  case class RandomInt() extends Op[Int32] { def mirror(f:Tx) = randomInt() }


  /** Rewrite rules **/
  rewrite[FixInv[_,_,_,_]]{case FixInv(Const(a: Int)) => lift(~a) }
  rewrite[FixNeg[_,_,_,_]]{case FixNeg(Const(a: Int)) => lift(-a) }
  rewrite[FixAdd[_,_,_,_]]{case FixAdd(Const(a: Int), Const(b: Int)) => lift(a + b) }
  rewrite[FixSub[_,_,_,_]]{case FixSub(Const(a: Int), Const(b: Int)) => lift(a - b) }
  rewrite[FixMul[_,_,_,_]]{case FixMul(Const(a: Int), Const(b: Int)) => lift(a * b) }
  rewrite[FixDiv[_,_,_,_]]{case FixDiv(Const(a: Int), Const(b: Int)) => lift(a / b) }
  rewrite[FixAnd[_,_,_,_]]{case FixAnd(Const(a: Int), Const(b: Int)) => lift(a & b) }
  rewrite[FixOr[_,_,_,_]]{ case  FixOr(Const(a: Int), Const(b: Int)) => lift(a | b) }
  rewrite[FixLt[_,_,_,_]]{ case  FixLt(Const(a: Int), Const(b: Int)) => lift(a < b) }
  rewrite[FixLeq[_,_,_,_]]{case FixLeq(Const(a: Int), Const(b: Int)) => lift(a <= b) }
  rewrite[FixNeq[_,_,_,_]]{case FixNeq(Const(a: Int), Const(b: Int)) => lift(a != b) }
  rewrite[FixEql[_,_,_,_]]{case FixEql(Const(a: Int), Const(b: Int)) => lift(a == b) }
  rewrite[FixMod[_,_,_]]{case FixMod(Const(a: Int), Const(b: Int)) => lift(a % b) }

  eval[RandomInt]{case _ => scala.util.Random.nextInt() }
}

