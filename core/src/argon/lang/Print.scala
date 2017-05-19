package argon.lang

import argon.compiler._
import argon.nodes._
import forge._

object PrintOps {
  /** Constructors **/
  @internal def print(x: Exp[MString]): Exp[MUnit] = stageSimple(Print(x))(ctx)
  @internal def println(x: Exp[MString]): Exp[MUnit] = stageSimple(Println(x))(ctx)
}

trait PrintExp {

}

trait PrintApi {
  /** Static methods **/
  @api def println(): MUnit = println("")

  @api def print[T:Type](x: T): MUnit = Unit(PrintOps.print(x.toText.s))
  @api def println[T:Type](x: T): MUnit = Unit(PrintOps.println(x.toText.s))

  @api def print(x: java.lang.String): MUnit = print(string2text(x))
  @api def println(x: java.lang.String): MUnit = println(string2text(x))
}


