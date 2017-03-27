package argon.ops

import argon.core.Staging

trait TextApi extends TextExp with BoolApi {
  type String = Text
}

trait TextExp extends Staging with BoolExp {
  /** Infix methods **/
  case class Text(s: Exp[Text]) extends MetaAny[Text] {
    def +(that: String)(implicit ctx: SrcCtx): Text = Text(text_concat(this.s, string2text(that).s))
    def +[T:Meta](that: T)(implicit ctx: SrcCtx): Text = Text(text_concat(this.s, textify(that).s))
    def !=(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_differ(this.s, that.s))
    def ==(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))

    def =!=(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_differ(this.s, that.s))
    def ===(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))
    def equals(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))
    def toText(implicit ctx: SrcCtx) = this
  }

  /** Direct methods **/
  def textify[T:Meta](x: T)(implicit ctx: SrcCtx): Text = Text(sym_tostring(x.s))


  /** Virtualized methods **/
  def infix_toString[S:Type](x: S)(implicit ctx: SrcCtx): Text = textify(x)
  def infix_+[R:Type](x1: String, x2: R)(implicit ctx: SrcCtx): Text = string2text(x1) + textify(x2)
  def infix_+[R:Type](x1: R, x2: String)(implicit ctx: SrcCtx): Text = textify(x1) + string2text(x2)

  /** Type classes **/
  // --- Staged
  implicit object TextType extends Meta[Text] {
    def wrapped(x: Exp[Text]) = Text(x)
    def stagedClass = classOf[Text]
    def isPrimitive = true
  }

  // --- Lift
  implicit object String2Text extends Lift[String,Text] {
    val staged = typ[Text]
    override def apply(x: String)(implicit ctx: SrcCtx) = Text(text(x))
  }

  /** Constant Lifting **/
  implicit def string2text(x: String): Text = lift(x)
  protected def text(x: String): Exp[Text] = constant[Text](x)

  /** IR Nodes **/
  case class ToString[S:Type](x: Exp[S]) extends Op[Text] { def mirror(f:Tx) = sym_tostring(f(x)) }
  case class TextConcat(x: Exp[Text], y: Exp[Text]) extends Op[Text] { def mirror(f:Tx) = text_concat(f(x),f(y)) }
  case class TextEquals(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_equals(f(x),f(y)) }
  case class TextDiffer(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_differ(f(x),f(y)) }

  /** Constructors **/
  def sym_tostring[S:Type](x: Exp[S])(implicit ctx: SrcCtx): Exp[Text] = x match {
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
