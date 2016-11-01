package argon.ops

import virtualized.EmbeddedControls

trait IfThenElseCore extends BoolCore with EmbeddedControls {
  /** Virtualized methods **/
  def __ifThenElse[S<:Sym:Typ](cond: Bool, thenp: => S, elsep: => S)(implicit ctx: SrcCtx): S = {
    val thenBlk = stageScope(thenp)
    val elseBlk = stageScope(elsep)
    val effects = thenBlk.summary orElse elseBlk.summary
    stageEffectful(IfThenElse(cond, thenBlk, elseBlk), effects)(ctx)
  }

  /** IR nodes **/
  case class IfThenElse[S<:Sym:Typ](cond: Bool, thenp: Block[S], elsep: Block[S]) extends Op[S] {
    def mirror(f:Tx) = __ifThenElse(f(cond), f(thenp), f(elsep))

    freqs   = normal(cond) ++ cold(thenp) ++ cold(elsep)
    aliases = List(thenp.result, elsep.result)
  }

  /** Rewrite rules **/
  rewrite[IfThenElse[_]]{
    case IfThenElse(Const(true),thenp,_)  => thenp.result
    case IfThenElse(Const(false),_,elsep) => elsep.result
    case e@IfThenElse(Def(Not(c)),thenp,elsep) => __ifThenElse(c,elsep.result,thenp.result)(e.mR, here)
  }
}

trait IfThenElseAPI extends IfThenElseCore
