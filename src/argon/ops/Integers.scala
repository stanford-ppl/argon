package argon.ops

import scala.language.implicitConversions


trait IntegerCore extends FixedPointCore {
  /** Staged types **/
  case class Int32() extends FixedPoint[Signed,B32,B0] { self =>
    override type LibType = Int
    override val tp = Int32Typ.asInstanceOf[Typ[self.type]]

    def unary_-(implicit ctx: SrcCtx) = neg[Signed,B32,B0,Int32](this)
    def unary_~(implicit ctx: SrcCtx) = inv[Signed,B32,B0,Int32](this)
    def + (that: Int32)(implicit ctx: SrcCtx) = add[Signed,B32,B0,Int32](this,that)
    def - (that: Int32)(implicit ctx: SrcCtx) = sub[Signed,B32,B0,Int32](this,that)
    def * (that: Int32)(implicit ctx: SrcCtx) = mul[Signed,B32,B0,Int32](this,that)
    def / (that: Int32)(implicit ctx: SrcCtx) = div[Signed,B32,B0,Int32](this,that)
    def & (that: Int32)(implicit ctx: SrcCtx) = and[Signed,B32,B0,Int32](this,that)
    def | (that: Int32)(implicit ctx: SrcCtx) =  or[Signed,B32,B0,Int32](this,that)
    def < (that: Int32)(implicit ctx: SrcCtx) =  lt[Signed,B32,B0,Int32](this,that)
    def <=(that: Int32)(implicit ctx: SrcCtx) = leq[Signed,B32,B0,Int32](this,that)
    def > (that: Int32)(implicit ctx: SrcCtx) =  lt[Signed,B32,B0,Int32](that,this)
    def >=(that: Int32)(implicit ctx: SrcCtx) = leq[Signed,B32,B0,Int32](that,this)
    def % (that: Int32)(implicit ctx: SrcCtx) = mod[Signed,B32,Int32](this,that)
  }
  implicit object Int32Typ extends FxPTyp[Int32] {
    override def next = Int32()
    override def typeArguments = Nil
    override def stagedClass = classOf[Int32]
    override def isPrimitive = true

    override def isSigned: Boolean = true
    override def intBits: Int = 32
    override def fracBits: Int = 0
    lazy val zero = fresh[Int32].asConst(0)
    lazy val one  = fresh[Int32].asConst(1)
  }

  /** Virtualized methods **/
  def infix_==(a: Int32, b: Int32)(implicit ctx: SrcCtx): Bool = eql[Signed,B32,B0,Int32](a,b)
  def infix_!=(a: Int32, b: Int32)(implicit ctx: SrcCtx): Bool = neq[Signed,B32,B0,Int32](a,b)
  def infix_equals(a: Int32, b: Int32)(implicit ctx: SrcCtx): Bool = eql[Signed,B32,B0,Int32](a,b)


  def isInt32[T:Typ] = typ[T] <:< Int32Typ
  implicit def lift(c: Int): Int32 = fresh[Int32].asConst(c)

  def randomInt()(implicit ctx: SrcCtx) = stageSimple(RandomInt())(ctx)

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

trait IntegerAPI extends IntegerCore with FixedPointAPI {
  type Int = Int32
}