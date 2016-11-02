package argon.ops

trait FixedPointCore extends BoolCore with NumCore {
  type Z = B0

  abstract class FxPTyp[T<:Sym] extends Num[T] {
    def isSigned: Boolean
    def intBits: Int
    def fracBits: Int
  }

  abstract class FixedPoint[S:Sign,I:Bits,F:Bits] extends Sym

  /*case class FltPt[G:Bits,E:Bits](init: Option[BigDecimal] = None) extends Sym { self =>
    override type LibType = BigDecimal
    val tp = fltPtTyp[G,E].asInstanceOf[Typ[self.type]]
  }
  implicit def fltPtTyp[G:Bits,E:Bits]: Typ[FltPt[G,E]] = new Typ[FltPt[G,E]] {
    override def next = new FltPt[G,E]()
    override def typeArguments = Nil
    override def stagedClass = classOf[FltPt[G,E]]
    override def isPrimitive = true
  }*/

  /** IR Nodes **/
  abstract class FixOp[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp] extends Op[FP] {
    val mFP: FxPTyp[FixedPoint[S,I,F]] = implicitly[FxPTyp[FP]].asInstanceOf[FxPTyp[FixedPoint[S,I,F]]]
    val mS = sign[S]
    val mI = bits[I]
    val mF = bits[F]
  }
  abstract class FixOp2[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp,R:Typ] extends Op[R] {
    val mFP: FxPTyp[FixedPoint[S,I,F]] = implicitly[FxPTyp[FP]].asInstanceOf[FxPTyp[FixedPoint[S,I,F]]]
    val mS = sign[S]
    val mI = bits[I]
    val mF = bits[F]
  }

  case class FixInv[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = inv[S,I,F,FP](f(x)) }
  case class FixNeg[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = neg[S,I,F,FP](f(x)) }

  case class FixAdd[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = add[S,I,F,FP](f(x),f(y)) }
  case class FixSub[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = sub[S,I,F,FP](f(x),f(y)) }
  case class FixMul[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = mul[S,I,F,FP](f(x),f(y)) }
  case class FixDiv[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = div[S,I,F,FP](f(x),f(y)) }
  case class FixAnd[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) = and[S,I,F,FP](f(x),f(y)) }
  case class FixOr [S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp[S,I,F,FP] { def mirror(f:Tx) =  or[S,I,F,FP](f(x),f(y)) }
  case class FixLt [S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp2[S,I,F,FP,Bool] { def mirror(f:Tx) =  lt[S,I,F,FP](f(x),f(y)) }
  case class FixLeq[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp2[S,I,F,FP,Bool] { def mirror(f:Tx) = leq[S,I,F,FP](f(x),f(y)) }
  case class FixNeq[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp2[S,I,F,FP,Bool] { def mirror(f:Tx) = neq[S,I,F,FP](f(x),f(y)) }
  case class FixEql[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP) extends FixOp2[S,I,F,FP,Bool] { def mirror(f:Tx) = eql[S,I,F,FP](f(x),f(y)) }
  case class FixMod[S:Sign,I:Bits,INT<:FixedPoint[S,I,Z]:FxPTyp](x: INT, y: INT) extends FixOp[S,I,Z,INT] { def mirror(f:Tx) = mod[S,I,INT](f(x),f(y)) }

  /** Internal ops **/
  def inv[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP)(implicit ctx: SrcCtx): FP = stage(FixInv[S,I,F,FP](x))(ctx)
  def neg[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP)(implicit ctx: SrcCtx): FP = stage(FixNeg[S,I,F,FP](x))(ctx)
  def add[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): FP = stage(FixAdd[S,I,F,FP](x,y))(ctx)
  def sub[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): FP = stage(FixSub[S,I,F,FP](x,y))(ctx)
  def mul[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): FP = stage(FixMul[S,I,F,FP](x,y))(ctx)
  def div[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): FP = stage(FixDiv[S,I,F,FP](x,y))(ctx)
  def and[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): FP = stage(FixAnd[S,I,F,FP](x,y))(ctx)
  def  or[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): FP = stage( FixOr[S,I,F,FP](x,y))(ctx)
  def  lt[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): Bool = stage( FixLt[S,I,F,FP](x,y))(ctx)
  def leq[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): Bool = stage(FixLeq[S,I,F,FP](x,y))(ctx)
  def neq[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): Bool = stage(FixNeq[S,I,F,FP](x,y))(ctx)
  def eql[S:Sign,I:Bits,F:Bits,FP<:FixedPoint[S,I,F]:FxPTyp](x: FP, y: FP)(implicit ctx: SrcCtx): Bool = stage(FixEql[S,I,F,FP](x,y))(ctx)

  def mod[S:Sign,I:Bits,INT<:FixedPoint[S,I,Z]:FxPTyp](x: INT, y: INT)(implicit ctx: SrcCtx): INT = stage(FixMod[S,I,INT](x,y))(ctx)

  rewrite[FixInv[_,_,_,_]]{ case FixInv(Def(FixInv(x))) => x }
  rewrite[FixNeg[_,_,_,_]]{ case FixNeg(Def(FixNeg(x))) => x }
  rewrite[FixAdd[_,_,_,_]]{
    case e@FixAdd(a, b) if b == e.mFP.zero => a
    case e@FixAdd(a, b) if a == e.mFP.zero => b
  }
  rewrite[FixSub[_,_,_,_]]{
    case e@FixSub(a, b) if b == e.mFP.zero => a
    //case e@FixSub(a, b) if a == e.mFP.zero => stage(FixNeg(b)(e.mS,e.mI,e.mF,e.mFP))(here)
  }
  rewrite[FixMul[_,_,_,_]]{
    case e@FixMul(a, b) if a == e.mFP.zero => e.mFP.zero
    case e@FixMul(a, b) if b == e.mFP.zero => e.mFP.zero
    case e@FixMul(a, b) if a == e.mFP.one  => b
    case e@FixMul(a, b) if b == e.mFP.one  => a
  }
  rewrite[FixDiv[_,_,_,_]]{
    //case e@FixDiv(a, b) if a == e.mR.zero => e.mR.zero // TODO: Unsafe!
    case e@FixDiv(a, b) if b == e.mFP.one => a
  }
  rewrite[FixAnd[_,_,_,_]]{
    case e@FixAnd(a, b) if a == e.mFP.zero => e.mFP.zero
    case e@FixAnd(a, b) if b == e.mFP.zero => e.mFP.zero
    // TODO: Identity
  }
  rewrite[FixOr[_,_,_,_]]{
    case e@FixOr(a, b) if a == e.mFP.zero => b
    case e@FixOr(a, b) if b == e.mFP.zero => a
    // TODO: Zero
  }

  rewrite[Not]{
    case Not(Def(e: FixNeq[s,i,f,fp])) => stage(FixEql[s,i,f,fp](e.x.asInstanceOf[fp],e.y.asInstanceOf[fp])(e.mS,e.mI,e.mF,e.mFP.asInstanceOf[FxPTyp[fp]]))(here)
    //case Not(Def(e@FixEql(x,y))) => uneraseFxP(e.mS,e.mI,e.mF,e.mFP){(mS,mI,mF,mFP) => stage(FixNeq(x,y)(mS,mI,mF,mFP)(here) }
    //case Not(Def(e@FixLt(x,y)))  => uneraseFxP(e.mS,e.mI,e.mF,e.mFP){(mS,mI,mF,mFP) => stage(FixLeq(x,y)(mS,mI,mF,mFP)(here) }
    //case Not(Def(e@FixLeq(x,y))) => uneraseFxP(e.mS,e.mI,e.mF,e.mFP){(mS,mI,mF,mFP) => stage(FixLt(x,y)(mS,mI,mF,mFP)(here) }
  }
}


trait FixedPointAPI extends FixedPointCore