package argon.nodes

import argon.core.compiler._
import argon.compiler._

/** IR Nodes **/
case class Assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]) extends Op[MUnit] {
  def mirror(f:Tx) = AssertOps.assert(f(cond),f(msg))
}
