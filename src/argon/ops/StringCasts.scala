package argon.ops

import argon.core.{Staging, ArgonExceptions}

trait StringCastApi extends StringCastExp with TextApi with MixedNumericApi {
  implicit class TextCastOps(x: Text) {
    def to[T <: StageAny[T] : FStaged](implicit ctx: SrcCtx): T = text_to_t[T](x)
  }
}

trait StringCastExp extends Staging with TextExp with MixedNumericExp with ArgonExceptions {
  /** Internals **/
  private[argon] def text_to_t[T <: StageAny[T] : FStaged](x: Text)(implicit ctx: SrcCtx): T = ftyp[T] match {
    case tp:FixPtType[s,i,f] =>
      implicit val mS = tp.mS.asInstanceOf[BOOL[s]]
      implicit val mI = tp.mI.asInstanceOf[INT[i]]
      implicit val mF = tp.mF.asInstanceOf[INT[f]]
      FixPt[s,i,f](text_to_fixpt[s,i,f](x.s)).asInstanceOf[T]
    case tp:FltPtType[g,e]   =>
      implicit val mG = tp.mG.asInstanceOf[INT[g]]
      implicit val mE = tp.mE.asInstanceOf[INT[e]]
      FltPt[g,e](text_to_fltpt[g,e](x.s)).asInstanceOf[T]
    case BoolType            => Bool(text_to_bool(x.s)).asInstanceOf[T]
    case tp =>
      new UnsupportedTextCastError(tp)
      wrap(fresh(tp)) // TODO: Is it necessary to distinguish this error symbol more explicitly?
  }

  /** IR Nodes **/
  case class StringToFixPt[S:BOOL,I:INT,F:INT](x: Exp[Text]) extends FixPtOp[S,I,F] {
    def mirror(f:Tx) = text_to_fixpt[S,I,F](x)
  }
  case class StringToFltPt[G:INT,E:INT](x: Exp[Text]) extends FltPtOp[G,E] {
    def mirror(f:Tx) = text_to_fltpt[G,E](x)
  }
  case class StringToBool(x: Exp[Text]) extends Op[Bool] {
    def mirror(f:Tx) = text_to_bool(x)
  }

  /** Smart Constructors **/
  def text_to_fixpt[S:BOOL,I:INT,F:INT](x: Exp[Text])(implicit ctx: SrcCtx): Exp[FixPt[S,I,F]] = x match {
    case Const(c: String) => string2fixpt[S,I,F](c).s
    case _ => stage(StringToFixPt[S,I,F](x))(ctx)
  }
  def text_to_fltpt[G:INT,E:INT](x: Exp[Text])(implicit ctx: SrcCtx): Exp[FltPt[G,E]] = x match {
    case Const(c: String) => string2fltpt[G,E](c).s
    case _ => stage(StringToFltPt[G,E](x))(ctx)
  }
  def text_to_bool(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Const("true") => bool(true)
    case Const("false") => bool(false)
    case _ => stage(StringToBool(x))(ctx)
  }
}
