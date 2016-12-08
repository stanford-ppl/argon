package argon.ops
import argon.core.Base
import scala.virtualized.SourceContext

trait Texts extends Base with BoolAPI {
  type Text <: TextOps
  protected trait TextOps {
    def +(that: Text)(implicit ctx: SrcCtx): Text
    def !=(that: Text)(implicit ctx: SrcCtx): Bool
    def ==(that: Text)(implicit ctx: SrcCtx): Bool
    def equals(that: Text)(implicit ctx: SrcCtx): Bool
  }

  implicit def lift(x: String): Text
  implicit val TextType: Staged[Text]
  def textify[S:Staged](s: S)(implicit ctx: SrcCtx): Text
}
trait TextAPI extends Texts {
  type String = Text
}


trait TextExp extends Texts with BoolExp {
  /** Staged type **/
  case class Text(s: Sym[Text]) extends TextOps {
    def +(that: Text)(implicit ctx: SrcCtx): Text = text_concat(this, that)(ctx)
    def !=(that: Text)(implicit ctx: SrcCtx): Bool = text_differ(this, that)(ctx)
    def ==(that: Text)(implicit ctx: SrcCtx): Bool = text_equals(this, that)(ctx)
    def equals(that: Text)(implicit ctx: SrcCtx): Bool = text_equals(this, that)(ctx)
  }
  implicit object TextType extends Staged[Text] {
    override def wrap(x: Sym[Text]) = Text(x)
    override def unwrap(x: Text) = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[Text]
    override def isPrimitive = true
  }

  /** Virtualized methods **/
  def infix_toString[S:Staged](x: S)(implicit ctx: SrcCtx): Text = textify(x)
  def infix_+[R:Staged](x1: String, x2: R)(implicit ctx: SrcCtx): Text = text_concat(lift(x1),textify(x2))(ctx)
  def infix_+[L:Staged](x1: L, x2: String)(implicit ctx: SrcCtx): Text = text_concat(textify(x1),lift(x2))(ctx)
  def infix_+[L:Staged,R:Staged](x1: L, x2: R)(implicit ctx: SrcCtx): Text = text_concat(textify(x1),textify(x2))(ctx)
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

  /** IR Nodes **/
  case class ToString[S:Staged](a: Sym[S]) extends Op[Text] { def mirror(f:Tx) = textify(f(a)) }
  case class TextConcat(a: Sym[Text], b: Sym[Text]) extends Op[Text] { def mirror(f:Tx) = text_concat(f(a),f(b)) }
  case class TextEquals(a: Sym[Text], b: Sym[Text]) extends Op[Bool] { def mirror(f:Tx) = text_equals(f(a),f(b)) }
  case class TextDiffer(a: Sym[Text], b: Sym[Text]) extends Op[Bool] { def mirror(f:Tx) = text_differ(f(a),f(b)) }

  /** Rewrite rules **/
  rewrite[ToString[_]]{ case ToString(Const(s)) => text(s.toString)  /* TODO: Always correct? */ }
  rewrite[TextConcat]{
    case TextConcat(Const(a: String), Const(b: String)) => text(a + b)
    case TextConcat(Const(""), y)                       => y
    case TextConcat(x, Const(""))                       => x
  }
  rewrite[TextEquals]{ case TextEquals(Const(a: String), Const(b: String)) => bool(a == b) }
  rewrite[TextDiffer]{ case TextDiffer(Const(a: String), Const(b: String)) => bool(a != b) }

  rewrite[Not]{ case Not(Def(TextEquals(a,b))) => text_differ(wrap(a),wrap(b)).s }
  rewrite[Not]{ case Not(Def(TextDiffer(a,b))) => text_equals(wrap(a),wrap(b)).s }

  /** Internal methods **/
  implicit def lift(x: String): Text = liftConst[Text](x)
  def text(x: String): Sym[Text] = liftConst[Text](x).s
  def textify[S:Staged](s: S)(implicit ctx: SrcCtx): Text = stage(ToString(unwrap(s)))(ctx)
  def text_concat(a: Text, b: Text)(implicit ctx: SrcCtx): Text = stage(TextConcat(a.s, b.s))(ctx)
  def text_equals(a: Text, b: Text)(implicit ctx: SrcCtx): Bool = stage(TextEquals(a.s, b.s))(ctx)
  def text_differ(a: Text, b: Text)(implicit ctx: SrcCtx): Bool = stage(TextDiffer(a.s, b.s))(ctx)
}
