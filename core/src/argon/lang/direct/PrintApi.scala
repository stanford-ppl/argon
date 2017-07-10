package argon.lang.direct

import argon.core._
import forge._

trait PrintApi {
  /** Static methods **/
  @api def println(): MUnit = println("")

  @api def print[T:Type](x: T): MUnit = MUnit(PrintOps.print(x.toText.s))
  @api def println[T:Type](x: T): MUnit = MUnit(PrintOps.println(x.toText.s))

  @api def print(x: String): MUnit = print(MString(x))
  @api def println(x: String): MUnit = println(MString(x))
}

