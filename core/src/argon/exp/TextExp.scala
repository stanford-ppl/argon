package argon.exp

import argon._
import forge._

case class Text(s: Exp[Text]) extends MetaAny[Text] {
  @api def +(rhs: String): Text = this + TextExp.lift(rhs)
  @api def +(rhs: Text): Text = wrap{ TextExp.concat(this.toText.s, rhs.s) }
  @api def +[R](rhs: MetaAny[R]): Text = wrap{ TextExp.concat(this.s, rhs.toText.s) }

  @api def =!=(that: Text): Bool = wrap{ TextExp.differ(this.s, that.s) }
  @api def ===(that: Text): Bool = wrap{ TextExp.equals(this.s, that.s) }
  @api def equals(that: Text): Bool = wrap{ TextExp.equals(this.s, that.s) }
  @api def toText = this
}

/** IR Nodes **/
case class ToString[T:Type](x: Exp[T]) extends Op[Text] {
  def mirror(f:Tx) = TextExp.sym_tostring(f(x))
  val mT = typ[T]
}
case class TextConcat(x: Exp[Text], y: Exp[Text]) extends Op[Text] { def mirror(f:Tx) = TextExp.concat(f(x),f(y)) }
case class TextEquals(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = TextExp.equals(f(x),f(y)) }
case class TextDiffer(x: Exp[Text], y: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = TextExp.differ(f(x),f(y)) }

object TextExp {
  @internal def lift(x: String): Text = Text(text(x))
  @internal def text(x: String): Exp[Text] = constant[Text](x)

  /** Constructors **/
  @internal def sym_tostring[T:Type](x: Exp[T]): Exp[Text] = x match {
    case Const(c: String) => text(c)
    case a if a.tp == TextType => a.asInstanceOf[Exp[Text]]
    case _ => stage(ToString(x))(ctx)
  }
  @internal def concat(x: Exp[Text], y: Exp[Text]): Exp[Text] = (x,y) match {
    case (Const(a: String), Const(b: String)) => text(a + b)
    case (Const(""), b) => b
    case (a, Const("")) => a
    case _ => stage( TextConcat(x,y) )(ctx)
  }
  @internal def equals(x: Exp[Text], y: Exp[Text]): Exp[Bool] = (x,y) match {
    case (Const(a: String), Const(b: String)) => bool(a == b)
    case _ => stage( TextEquals(x,y) )(ctx)
  }
  @internal def differ(x: Exp[Text], y: Exp[Text]): Exp[Bool] = (x,y) match {
    case (Const(a: String), Const(b: String)) => bool(a == b)
    case _ => stage( TextDiffer(x,y) )(ctx)
  }
}