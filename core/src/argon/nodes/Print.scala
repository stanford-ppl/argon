package argon.nodes

import argon.core.compiler._
import argon.lang.compiler._

/** IR Nodes **/
case class Print(x: Exp[MString]) extends Op[MUnit] { def mirror(f:Tx) = PrintOps.print(f(x)) }
case class Println(x: Exp[MString]) extends Op[MUnit] { def mirror(f:Tx) = PrintOps.println(f(x)) }
