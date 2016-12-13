package argon.ops

/** Casting between FixPt and FltPt **/
trait MixedNumerics extends Casts with FixPts with FltPts {

}

trait MixedNumericApi extends Casts with FixPtApi with FltPtApi


trait MixedNumericExp extends Casts with FixPtExp with FltPtExp {

  override protected def cast[T:Num,R:Num](x: T)(implicit ctx: SrcCtx): R = (num[T],num[R]) match {
    case (a: FixPtType[s,i,f], b: FltPtType[g,e]) =>
      implicit val mS: BOOL[s] = a.mS
      implicit val mI: INT[i] = a.mI
      implicit val mF: INT[f] = a.mF
      implicit val mG: INT[g] = b.mG.asInstanceOf[INT[g]]
      implicit val mE: INT[e] = b.mE.asInstanceOf[INT[e]]
      wrap(fix_to_flt[s,i,f,g,e](x.asInstanceOf[FixPt[s,i,f]].s)).asInstanceOf[R]

    case (a: FltPtType[g,e], b: FixPtType[s,i,f]) =>
      implicit val mS: BOOL[s] = b.mS
      implicit val mI: INT[i] = b.mI
      implicit val mF: INT[f] = b.mF
      implicit val mG: INT[g] = a.mG.asInstanceOf[INT[g]]
      implicit val mE: INT[e] = a.mE.asInstanceOf[INT[e]]
      wrap(flt_to_fix[g,e,s,i,f](x.asInstanceOf[FltPt[g,e]].s)).asInstanceOf[R]

    case _ => super.cast[T,R](x)
  }

  /** IR Nodes **/
  case class FixPtToFltPt[S:BOOL,I:INT,F:INT,G:INT,E:INT](x: Sym[FixPt[S,I,F]]) extends FltPtOp[G,E] {
    def mirror(f:Tx) = fix_to_flt[S,I,F,G,E](f(x))
  }

  case class FltPtToFixPt[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Sym[FltPt[G,E]]) extends FixPtOp[S,I,F] {
    def mirror(f:Tx) = flt_to_fix[G,E,S,I,F](f(x))
  }


  /** Constructors **/
  def fix_to_flt[S:BOOL,I:INT,F:INT,G:INT,E:INT](x: Sym[FixPt[_,_,_]])(implicit ctx: SrcCtx): Sym[FltPt[G,E]] = {
    stage(FixPtToFltPt[S,I,F,G,E](x.asInstanceOf[Sym[FixPt[S,I,F]]]))(ctx)
  }
  def flt_to_fix[G:INT,E:INT,S:BOOL,I:INT,F:INT](x: Sym[FltPt[_,_]])(implicit ctx: SrcCtx): Sym[FixPt[S,I,F]] = {
    stage(FltPtToFixPt[G,E,S,I,F](x.asInstanceOf[Sym[FltPt[G,E]]]))(ctx)
  }
}