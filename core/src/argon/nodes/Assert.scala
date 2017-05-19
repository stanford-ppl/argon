package argon.nodes

import argon.compiler._
import argon.lang.AssertOps

/** IR Nodes **/
case class Assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]) extends Op[MUnit] {
  def mirror(f:Tx) = AssertOps.assert(f(cond),f(msg))
}
