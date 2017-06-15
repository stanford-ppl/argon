package argon.nodes

import argon.internals._
import argon.compiler._

/** IR Nodes **/
case class Print(x: Exp[MString]) extends Op[MUnit] { def mirror(f:Tx) = PrintOps.print(f(x)) }
case class Println(x: Exp[MString]) extends Op[MUnit] { def mirror(f:Tx) = PrintOps.println(f(x)) }
