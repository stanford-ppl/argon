package argon.ops

import argon.core.Staging

import forge._

trait AssertApi extends AssertExp with BoolApi with TextApi with VoidApi {
  @api def assert(cond: Bool, msg: Text): Void = Void(stage_assert(cond.s, Some(msg.s)))
  @api def assert(cond: Bool): Void = Void(stage_assert(cond.s, None))
}

trait AssertExp extends Staging with BoolExp with TextExp with VoidExp {
  /** IR Nodes **/
  case class Assert(cond: Exp[Bool], msg: Option[Exp[Text]]) extends Op[Void] {
    def mirror(f:Tx) = stage_assert(f(cond),f(msg))
  }

  /** Constructors **/
  def stage_assert(cond: Exp[Bool], msg: Option[Exp[Text]])(implicit ctx: SrcCtx): Sym[Void] = {
    stageGlobal(Assert(cond,msg))(ctx)
  }
}
