package argon.ops

import argon.{ArgonApi, ArgonExp}
import argon.core.Staging
import forge._

trait TextApi extends TextExp {
  self: ArgonApi =>

  type String = Text

}

trait TextExp extends BoolExp {
  self: ArgonExp =>
  /** Infix methods **/
  implicit object TextType extends Meta[Text] {
    def wrapped(x: Exp[Text]) = Text(x)
    def stagedClass = classOf[Text]
    def isPrimitive = true
  }

  case class Text(s: Exp[Text]) extends MetaAny[Text] {
    @api def +(rhs: String): Text = concat(this.toText, liftString(rhs))
    @api def +(rhs: Text): Text = concat(this.toText, rhs)
    @api def +[R](rhs: MetaAny[R]): Text = concat(this, rhs.toText)

    @api def =!=(that: Text): Bool = Bool(text_differ(this.s, that.s))
    @api def ===(that: Text): Bool = Bool(text_equals(this.s, that.s))
    @api def equals(that: Text): Bool = Bool(text_equals(this.s, that.s))
    @api def toText = this
  }

  /** Direct methods **/
  def textify[T:Meta](x: T)(implicit ctx: SrcCtx): Text = Text(sym_tostring(x.s))
  def liftString(x: String)(implicit ctx: SrcCtx): Text = string2text(x)
  def concat(x: Text, y: Text)(implicit ctx: SrcCtx): Text = Text(text_concat(x.s, y.s))

  /** Virtualized methods **/
  @util def infix_+[R<:MetaAny[R]](x1: String, x2: R): Text = string2text(x1) + x2.toText

  /*
    // Has to be an implicit class to not conflict with higher priority implicits on +
  implicit class ConcatOps[T<:MetaAny[T]](lhs: T) {
    @api def +(rhs: String): Text = concat(lhs.toText, liftString(rhs))
    @api def +(rhs: Text): Text = concat(lhs.toText, rhs)
    @api def +[R](rhs: MetaAny[R]): Text = concat(lhs.toText, rhs.toText)
  }

  */

  // Never created
  //@util def infix_+[R<:MetaAny[R]](x1: R, x2: String): Text = textify(x1) + string2text(x2)

  /** Type classes **/
  // --- Staged

  // --- Lift
  implicit object LiftString2Text extends Lift[String,Text] {
    override def apply(x: String)(implicit ctx: SrcCtx) = Text(text(x))
  }
  implicit object CastString2Text extends Cast[String,Text] {
    override def apply(x: String)(implicit ctx: SrcCtx) = Text(text(x))
  }

  /** Constant Lifting **/
  implicit def string2text(x: String)(implicit ctx: SrcCtx): Text = lift(x)
  protected def text(x: String)(implicit ctx: SrcCtx): Exp[Text] = constant[Text](x)

  /** IR Nodes **/
  case class ToString[T:Type](x: Exp[T]) extends Op[Text] {
    def mirror(f:Tx) = sym_tostring(f(x))
    val mT = typ[T]
  }
  case class TextConcat(x: Exp[Text], y: Exp[Text]) extends Op[Text] { def mirror(f:Tx) = text_concat(f(x),f(y)) }
  case class TextEquals(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_equals(f(x),f(y)) }
  case class TextDiffer(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = text_differ(f(x),f(y)) }

  /** Constructors **/
  def sym_tostring[T:Type](x: Exp[T])(implicit ctx: SrcCtx): Exp[Text] = x match {
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
