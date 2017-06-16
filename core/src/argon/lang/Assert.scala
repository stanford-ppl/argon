package argon.lang

import argon.core._
import argon.nodes._
import forge._

object AssertOps {
  @internal def assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]): Sym[MUnit] = stageGlobal(Assert(cond,msg))(ctx)
}

trait AssertApi {
  /** Static methods **/
  @api def assert(cond: MBoolean, msg: MString): MUnit = Unit(AssertOps.assert(cond.s, Some(msg.s)))
  @api def assert(cond: MBoolean): MUnit = Unit(AssertOps.assert(cond.s, None))
}


