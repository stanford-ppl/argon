package argon.ops
import argon.core.Base

trait IfThenElseOps extends Base with Bools
trait IfThenElseAPI extends IfThenElseOps with BoolAPI

trait IfThenElseExp extends IfThenElseOps with BoolExp {
  /** Virtualized methods **/
  def __ifThenElse[T:Staged](cond: Bool, thenp: => T, elsep: => T)(implicit ctx: SrcCtx): T = {
    val thenBlk = stageScope(thenp)
    val elseBlk = stageScope(elsep)
    val effects = thenBlk.summary orElse elseBlk.summary
    stageEffectful(IfThenElse(unwrap(cond), thenBlk, elseBlk), effects)(ctx)
  }

  /** IR nodes **/
  case class IfThenElse[T:Staged](cond: Sym[Bool], thenp: Block[T], elsep: Block[T]) extends Op[T] {
    def mirror(f:Tx) = __ifThenElse(f(cond), f(thenp), f(elsep))

    freqs   = normal(cond) ++ cold(thenp) ++ cold(elsep)
    aliases = List(thenp.getResult, elsep.getResult)
  }

  /** Rewrite rules **/
  rewrite[IfThenElse[_]]{
    case IfThenElse(Const(true),thenp,_)  => thenp.inline
    case IfThenElse(Const(false),_,elsep) => elsep.inline
    case e@IfThenElse(Def(Not(c)),thenp,elsep) => unwrap(__ifThenElse(wrap(c),elsep.result,thenp.result)(e.mR, here))(e.mR)
  }
}
