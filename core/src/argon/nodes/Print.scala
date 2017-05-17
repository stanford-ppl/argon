package argon.nodes

import argon._

/** IR Nodes **/
case class Print(x: Exp[MString]) extends Op[MUnit] { def mirror(f:Tx) = lang.PrintOps.print(f(x)) }
case class Println(x: Exp[MString]) extends Op[MUnit] { def mirror(f:Tx) = lang.PrintOps.println(f(x)) }
