package argon.lang

import argon._
import forge._

private object AssertOps {
  @internal def assert(cond: Exp[Bool], msg: Option[Exp[Text]]): Sym[Void] = stageGlobal(Assert(cond,msg))(ctx)
}

trait AssertsExp {
  /** Static methods **/
  @api def assert(cond: Bool, msg: Text): Void = Void(AssertOps.assert(cond.s, Some(msg.s)))
  @api def assert(cond: Bool): Void = Void(AssertOps.assert(cond.s, None))
}

/** IR Nodes **/
case class Assert(cond: Exp[Bool], msg: Option[Exp[Text]]) extends Op[Void] {
  def mirror(f:Tx) = AssertOps.assert(f(cond),f(msg))
}
