package argon.ops

import argon.core.Staging
import org.virtualized.{SourceContext, stageany}

trait TextApi extends TextExp with BoolApi {
  type String = Text
}

@stageany
trait TextExp extends Staging with BoolExp {
  /** Infix methods **/
  case class Text(s: Exp[Text]) extends StageAny[Text] {
    def +(that: String)(implicit ctx: SrcCtx): Text = Text(text_concat(this.s, string2text(that).s))
    def +[T:StageAny](that: T)(implicit ctx: SrcCtx): Text = Text(text_concat(this.s, that.toText.s))
    def =!=(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_differ(this.s, that.s))
    def ===(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))
    def equals(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))

    override def toText(implicit ctx: SrcCtx) = this
  }

  def textify[T:StageAny](x: T)(implicit ctx: SrcCtx): Text = Text(sym_tostring(x.s))

  /** Type classes **/
  // --- Staged
  implicit object TextType extends FStaged[Text] {
    override def wrapped(x: Exp[Text]) = Text(x)
    override def typeArguments = Nil
    override def stagedClass = classOf[Text]
    override def isPrimitive = true
  }

  // --- Lift
  implicit object String2Text extends Lift[String,Text] {
    val staged = btyp[Text]
    override def lift(x: String)(implicit ctx: SrcCtx) = Text(text(x))
  }

  /** Constant Lifting **/
  implicit def string2text(x: String): Text = lift(x)
  protected def text(x: String): Exp[Text] = constant[Text](x)

  /** IR Nodes **/
  case class ToString[S:StageAny](x: Exp[S]) extends Op[Text] { def mirror(f:Tx) = sym_tostring(f(x)) }
  case class TextConcat(x: Exp[Text], y: Exp[Text]) extends Op[Text] { def mirror(f:Tx) = text_concat(f(x),f(y)) }
  case class TextEquals(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_equals(f(x),f(y)) }
  case class TextDiffer(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_differ(f(x),f(y)) }

  /** Constructors **/
  def sym_tostring[S:StageAny](x: Exp[S])(implicit ctx: SrcCtx): Exp[Text] = x match {
    case Const(c: String) => text(c)
    case a if a.tp == TextType => a.asInstanceOf[Exp[Text]]
    case _ => stage(ToString(x))(ctx)
  }
  def text_concat(x: Exp[Text], y: Exp[Text])(implicit ctx: SrcCtx): Exp[Text] = (x,y) match {
    case (Const(a: String), Const(b: String)) => text(a + b)
    case (Const(""), b) => b
    case (a, Const("")) => a
    case _ => stage( TextConcat(x,y) )(ctx)
  }
  def text_equals(x: Exp[Text], y: Exp[Text])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a: String), Const(b: String)) => bool(a == b)
    case _ => stage( TextEquals(x,y) )(ctx)
  }
  def text_differ(x: Exp[Text], y: Exp[Text])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a: String), Const(b: String)) => bool(a == b)
    case _ => stage( TextDiffer(x,y) )(ctx)
  }

  /** Rewrite Rules **/
  override def bool_not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Op(TextEquals(a,b)) => text_differ(a,b)
    case Op(TextDiffer(a,b)) => text_equals(a,b)
    case _ => super.bool_not(x)
  }
}
