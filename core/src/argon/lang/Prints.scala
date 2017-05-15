package argon.lang

import argon._
import forge._

private object PrintOps {
  /** Constructors **/
  @internal def print(x: Exp[Text]): Exp[Void] = stageSimple(Print(x))(ctx)
  @internal def println(x: Exp[Text]): Exp[Void] = stageSimple(Println(x))(ctx)
}

trait PrintsExp {
  /** Static methods **/
  @api def println(): Void = println("")

  @api def print[T:Type](x: T): Void = Void(PrintOps.print(x.toText.s))
  @api def println[T:Type](x: T): Void = Void(PrintOps.println(x.toText.s))

  @api def print(x: java.lang.String): Void = print(string2text(x))
  @api def println(x: java.lang.String): Void = println(string2text(x))
}

/** IR Nodes **/
case class Print(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = PrintOps.print(f(x)) }
case class Println(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = PrintOps.println(f(x)) }
