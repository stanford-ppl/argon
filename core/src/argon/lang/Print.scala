package argon.lang

import argon.core.compiler._
import argon.nodes._
import forge._

object PrintOps {
  /** Constructors **/
  @internal def print(x: Exp[MString]): Exp[MUnit] = stageSimple(Print(x))(ctx)
  @internal def println(x: Exp[MString]): Exp[MUnit] = stageSimple(Println(x))(ctx)
}

trait PrintApi {
  /** Static methods **/
  @api def println(): MUnit = println("")

  @api def print[T:Type](x: T): MUnit = Unit(PrintOps.print(x.toText.s))
  @api def println[T:Type](x: T): MUnit = Unit(PrintOps.println(x.toText.s))

  @api def print(x: CString): MUnit = print(String(x))
  @api def println(x: CString): MUnit = println(String(x))
}


