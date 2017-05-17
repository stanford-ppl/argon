package argon.nodes

import argon._

/** IR Nodes **/
case class Assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]) extends Op[MUnit] {
  def mirror(f:Tx) = lang.AssertOps.assert(f(cond),f(msg))
}
