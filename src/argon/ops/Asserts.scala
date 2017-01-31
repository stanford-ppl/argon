package argon.ops

/**
  * Created by david on 1/31/17.
  */
trait AssertOps extends BoolOps with TextOps with VoidOps {

  private[argon] def assert_impl(cond: Bool, msg: Option[Text])(implicit ctx: SrcCtx): Void
}
trait AssertApi extends AssertOps with BoolApi with TextApi with VoidApi {
  def assert(cond: Bool, msg: Text)(implicit ctx: SrcCtx): Void = assert_impl(cond, Some(msg))
  def assert(cond: Bool)(implicit ctx: SrcCtx): Void = assert_impl(cond, None)
}
trait AssertExp extends AssertOps with BoolExp with TextExp with VoidExp {

  private[argon] def assert_impl(cond: Bool, msg: Option[Text])(implicit ctx: SrcCtx): Void = {
    Void(stage_assert(cond.s, msg.map(_.s)))
  }

  /** IR Nodes **/
  case class Assert(cond: Exp[Bool], msg: Option[Exp[Text]]) extends Op[Void] {
    def mirror(f:Tx) = stage_assert(f(cond),f(msg))
  }

  def stage_assert(cond: Exp[Bool], msg: Option[Exp[Text]])(implicit ctx: SrcCtx): Sym[Void] = {
    stageGlobal(Assert(cond,msg))(ctx)
  }
}
