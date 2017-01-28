package argon.ops
import argon.core.Base
import org.virtualized.SourceContext

trait TextOps extends Base with BoolOps {
  type Text <: TextOps
  protected trait TextOps {
    def +(that: Text)(implicit ctx: SrcCtx): Text
    def +[T:Staged](that: T)(implicit ctx: SrcCtx): Text
    def +(that: String)(implicit ctx: SrcCtx): Text = this + string2text(that)
    def !=(that: Text)(implicit ctx: SrcCtx): Bool
    def ==(that: Text)(implicit ctx: SrcCtx): Bool
    def equals(that: Text)(implicit ctx: SrcCtx): Bool
  }
  def infix_+[R:Staged](x1: String, x2: R)(implicit ctx: SrcCtx): Text

  implicit object String2Text extends Lift[String,Text] { val staged = TextType }
  implicit def string2text(x: String): Text = lift(x)
  implicit val TextType: Staged[Text]
  def textify[T:Staged](x: T)(implicit ctx: SrcCtx): Text
}
trait TextApi extends TextOps with BoolApi {
  type String = Text
}


trait TextExp extends TextOps with BoolExp {
  /** API **/
  case class Text(s: Exp[Text]) extends TextOps {
    def +(that: Text)(implicit ctx: SrcCtx): Text = Text(text_concat(this.s, that.s))
    def +[T:Staged](that: T)(implicit ctx: SrcCtx): Text = Text(text_concat(this.s, textify(that).s))
    def !=(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_differ(this.s, that.s))
    def ==(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))
    def equals(that: Text)(implicit ctx: SrcCtx): Bool = Bool(text_equals(this.s, that.s))
  }

  def textify[T:Staged](x: T)(implicit ctx: SrcCtx): Text = Text(sym_tostring(x.s))

  /** virtualized methods **/
  def infix_toString[S:Staged](x: S)(implicit ctx: SrcCtx): Text = textify(x)
  def infix_+[R:Staged](x1: String, x2: R)(implicit ctx: SrcCtx): Text = string2text(x1) + textify(x2)

  // These are currently never created...
  // def infix_+[L:Staged](x1: L, x2: String)(implicit ctx: SrcCtx): Text = textify(x1) + string2text(x2)
  // def infix_+[L:Staged,R:Staged](x1: L, x2: R)(implicit ctx: SrcCtx): Text = textify(x1) + textify(x2)

  def infix_==(x: Text, a: Any)(implicit ctx: SrcCtx): Bool = a match {
    case y: Text   => x == y
    case s: String => x == string2text(s)
    case _         => boolean2bool(false)
  }
  def infix_!=(x: Text, a: Any)(implicit ctx: SrcCtx): Bool = a match {
    case y: Text   => x != y
    case s: String => x != string2text(s)
    case _         => boolean2bool(false)
  }
  def infix_==(s: String, b: Text)(implicit ctx: SrcCtx): Bool = string2text(s) == b
  def infix_!=(s: String, b: Text)(implicit ctx: SrcCtx): Bool = string2text(s) != b


  /** Staged Type **/
  implicit object TextType extends Staged[Text] {
    override def wrapped(x: Exp[Text]) = Text(x)
    override def unwrapped(x: Text) = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[Text]
    override def isPrimitive = true
  }


  /** Constant Lifting **/
  def text(x: String): Exp[Text] = constant[Text](x)

  /** IR Nodes **/
  case class ToString[S:Staged](x: Exp[S]) extends Op[Text] { def mirror(f:Tx) = sym_tostring(f(x)) }
  case class TextConcat(x: Exp[Text], y: Exp[Text]) extends Op[Text] { def mirror(f:Tx) = text_concat(f(x),f(y)) }
  case class TextEquals(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_equals(f(x),f(y)) }
  case class TextDiffer(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_differ(f(x),f(y)) }

  /** Smart Constructors **/
  def sym_tostring[S:Staged](x: Exp[S])(implicit ctx: SrcCtx): Exp[Text] = x match {
    case Const(c: String) => text(c)
    //case Const(c) => text(c.toString)
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
