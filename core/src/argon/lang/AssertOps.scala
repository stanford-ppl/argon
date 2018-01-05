package argon.lang

import argon.core._
import argon.nodes._
import forge._

object AssertOps {
  @internal def assert(cond: Exp[MBoolean], msg: Option[Exp[MString]]): Exp[MUnit] = cond match {
    case Const(true) => Unit.const()
    case _ => stageGlobal(Assert(cond,msg))(ctx)
  }
}
