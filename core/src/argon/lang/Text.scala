package argon.lang

import argon._
import forge._

case class Text(s: Exp[Text]) extends MetaAny[Text] {
  @api def +(rhs: String): Text = this + Text(rhs)
  @api def +(rhs: Text): Text = Text(Text.concat(this.toText.s, rhs.s))
  @api def +[R](rhs: MetaAny[R]): Text = Text(Text.concat(this.s, rhs.toText.s))

  @api def =!=(that: Text): Bool = Bool(Text.differ(this.s, that.s))
  @api def ===(that: Text): Bool = Bool(Text.equals(this.s, that.s))
  @api def equals(that: Text): Bool = Bool(Text.equals(this.s, that.s))
  @api def toText = this
}

object TextType extends Type[Text] {
  def wrapped(x: Exp[Text]) = Text(x)
  def stagedClass = classOf[Text]
  def isPrimitive = true
}

object Text {
  @api def apply(s: String): Text = Text(const(s))
  @internal def const(s: String): Const[Text] = constant[Text](s)

  @internal def ify[T:Type](x: T): Text = Text(sym_tostring(x.s))

  /** Constructors **/
  @internal def sym_tostring[T:Type](x: Exp[T]): Exp[Text] = x match {
    case Const(c: String) => Text.const(c)
    case a if a.tp == TextType => a.asInstanceOf[Exp[Text]]
    case _ => stage(ToString(x))(ctx)
  }
  @internal def concat(x: Exp[Text], y: Exp[Text]): Exp[Text] = (x,y) match {
    case (Const(a: String), Const(b: String)) => Text.const(a + b)
    case (Const(""), b) => b
    case (a, Const("")) => a
    case _ => stage( TextConcat(x,y) )(ctx)
  }
  @internal def equals(x: Exp[Text], y: Exp[Text]): Exp[Bool] = (x,y) match {
    case (Const(a: String), Const(b: String)) => Bool.const(a == b)
    case _ => stage( TextEquals(x,y) )(ctx)
  }
  @internal def differ(x: Exp[Text], y: Exp[Text]): Exp[Bool] = (x,y) match {
    case (Const(a: String), Const(b: String)) => Bool.const(a == b)
    case _ => stage( TextDiffer(x,y) )(ctx)
  }


}

trait TextExp {
  /** Static methods **/
  @internal def infix_+[R<:MetaAny[R]](x1: String, x2: R): Text = Text(x1) + x2.toText

  /** Rewrite Rules **/
  /*@rewrite def Bool$not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Op(TextEquals(a,b)) => text_differ(a,b)
    case Op(TextDiffer(a,b)) => text_equals(a,b)
  }*/

  /** Type classes **/
  implicit val textIsStaged: Type[Text] = TextType

  /** Lifting **/
  implicit def string2text(x: String)(implicit ctx: SrcCtx): Text = Text(x)

  implicit object LiftString2Text extends Lift[String,Text] {
    @internal def apply(x: String): Text = Text(x)
  }

  /** Casting **/
  implicit object CastString2Text extends Cast[String,Text] {
    @internal def apply(x: String): Text = Text(x)
  }
}

trait TextApi {
  type String = Text
}

/** IR Nodes **/
case class ToString[T:Type](x: Exp[T]) extends Op[Text] {
  def mirror(f:Tx) = Text.sym_tostring(f(x))
  val mT = typ[T]
}
case class TextConcat(x: Exp[Text], y: Exp[Text]) extends Op[Text] { def mirror(f:Tx) = Text.concat(f(x),f(y)) }
case class TextEquals(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = Text.equals(f(x),f(y)) }
case class TextDiffer(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = Text.differ(f(x),f(y)) }
