package argon.lang

import argon.core._
import argon.nodes._
import forge._

object AssertOps {
  @internal def assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]): Sym[MUnit] = stageGlobal(Assert(cond,msg))(ctx)
}
