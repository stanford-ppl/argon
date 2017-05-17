package argon.lang

import argon._
import argon.nodes._
import forge._

object AssertOps {
  @internal def assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]): Sym[MUnit] = stageGlobal(Assert(cond,msg))(ctx)
}

trait AssertExp {
  /** Static methods **/
  @api def assert(cond: MBoolean, msg: MString): MUnit = MUnit(AssertOps.assert(cond.s, Some(msg.s)))
  @api def assert(cond: MBoolean): MUnit = MUnit(AssertOps.assert(cond.s, None))
}


