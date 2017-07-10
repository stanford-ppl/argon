package argon.lang

import argon.core._
import argon.nodes._
import forge._

object PrintOps {
  /** Constructors **/
  @internal def print(x: Exp[MString]): Exp[MUnit] = stageSimple(Print(x))(ctx)
  @internal def println(x: Exp[MString]): Exp[MUnit] = stageSimple(Println(x))(ctx)
}
