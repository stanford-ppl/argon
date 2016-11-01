package argon.ops

import scala.language.implicitConversions
import virtualized.EmbeddedControls
import virtualized.SourceContext

trait TextCore extends BoolCore with EmbeddedControls {
  /** Staged type **/
  case class Text() extends Sym {
    override type LibType = String
    def tp = TextType.asInstanceOf[Typ[this.type]]
  }
  implicit object TextType extends Typ[Text] {
    override def next: Text = Text()
    override def typeArguments = Nil
    override def stagedClass = classOf[Text]
    override def isPrimitive = true
  }

  /** Virtualized methods **/
  def infix_toString[S:Typ](x: S)(implicit ctx: SrcCtx): Text = textify(x)
  def infix_+[R:Typ](x1: String, x2: R)(implicit ctx: SrcCtx): Text = text_concat(lift(x1),textify(x2))(ctx)
  def infix_+[L:Typ](x1: L, x2: String)(implicit ctx: SrcCtx): Text = text_concat(textify(x1),lift(x2))(ctx)
  def infix_+[L:Typ,R:Typ](x1: L, x2: R)(implicit ctx: SrcCtx): Text = text_concat(textify(x1),textify(x2))(ctx)
  def infix_==(x: Text, a: Any)(implicit ctx: SrcCtx): Bool = a match {
    case y: Text   => text_equals(x, y)(ctx)
    case s: String => text_equals(x, lift(s))(ctx)
    case _         => lift(false)
  }
  def infix_equals(x: Text, a: Any)(implicit ctx: SrcCtx): Bool = infix_==(x, a)(ctx)
  def infix_!=(x: Text, a: Any)(implicit ctx: SrcCtx): Bool = a match {
    case y: Text   => text_differ(x, y)(ctx)
    case s: String => text_differ(x, lift(s))(ctx)
    case _         => lift(false)
  }
  def infix_==(s: String, b: Text)(implicit ctx: SrcCtx): Bool = text_equals(lift(s), b)(ctx)
  def infix_!=(s: String, b: Text)(implicit ctx: SrcCtx): Bool = text_differ(lift(s), b)(ctx)
  def infix_equals(s: String, b: Text)(implicit ctx: SrcCtx): Bool = text_equals(lift(s), b)(ctx)

  /** Implicit lifting **/
  implicit def lift(x: String): Text = fresh[Text].asConst(x)

  /** IR Nodes **/
  case class ToString[S:Typ](a: S) extends Op[Text] { def mirror(f:Tx) = textify(f(a)) }
  case class TextConcat(a: Text, b: Text) extends Op[Text] { def mirror(f:Tx) = text_concat(f(a),f(b)) }
  case class TextEquals(a: Text, b: Text) extends Op[Bool] { def mirror(f:Tx) = text_equals(f(a),f(b)) }
  case class TextDiffer(a: Text, b: Text) extends Op[Bool] { def mirror(f:Tx) = text_differ(f(a),f(b)) }

  /** Rewrite rules **/
  rewrite[ToString[_]]{ case ToString(Const(s)) => lift(s.toString)  /* TODO: Always correct? */ }
  rewrite[TextConcat]{
    case TextConcat(Const(a: String), Const(b: String)) => lift(a + b)
    case TextConcat(Const(""), y)                       => y
    case TextConcat(x, Const(""))                       => x
  }
  rewrite[TextEquals]{ case TextEquals(Const(a: String), Const(b: String)) => lift(a == b) }
  rewrite[TextDiffer]{ case TextDiffer(Const(a: String), Const(b: String)) => lift(a != b) }

  rewrite[Not]{ case Not(Def(TextEquals(a,b))) => text_differ(a,b) }
  rewrite[Not]{ case Not(Def(TextDiffer(a,b))) => text_equals(a,b) }

  /** Internal methods **/
  def textify[S:Typ](s: S)(implicit ctx: SrcCtx): Text = stage(ToString(s))(ctx)
  def text_concat(a: Text, b: Text)(implicit ctx: SrcCtx): Text = stage(TextConcat(a, b))(ctx)
  def text_equals(a: Text, b: Text)(implicit ctx: SrcCtx): Bool = stage(TextEquals(a,b))(ctx)
  def text_differ(a: Text, b: Text)(implicit ctx: SrcCtx): Bool = stage(TextDiffer(a,b))(ctx)
}

trait TextAPI extends TextCore {
  type String = Text
}
