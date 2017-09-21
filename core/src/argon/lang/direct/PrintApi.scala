package argon.lang.direct

import argon.core._
import forge._

trait PrintApi {
  /** Static methods **/

  /** Prints an empty line to the console. **/
  @api def println(): MUnit = println("")

  /** Prints a String representation of the given value to the console. **/
  @api def print[T:Type](x: T): MUnit = MUnit(PrintOps.print(x.toText.s))
  /** Prints a String representation of the given value to the console followed by a linebreak. **/
  @api def println[T:Type](x: T): MUnit = MUnit(PrintOps.println(x.toText.s))

  /** Prints the given String to the console. **/
  @api def print(x: String): MUnit = print(MString(x))
  /** Prints the given String to the console, followed by a linebreak. **/
  @api def println(x: String): MUnit = println(MString(x))
}

