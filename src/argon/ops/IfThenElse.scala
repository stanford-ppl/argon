package argon.ops
import argon.core.Base

trait IfThenElseOps extends Base with BoolOps
trait IfThenElseApi extends IfThenElseOps with BoolApi

trait IfThenElseExp extends IfThenElseOps with BoolExp {
  /** Virtualized methods **/
  def __ifThenElse[A,B](cond: Bool, thenp: => A, elsep: => A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = {
    implicit val staged: Staged[B] = l.staged
    val unwrapThen = () => unwrap(lift(thenp) ) // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => unwrap(lift(elsep) ) // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  /** IR Nodes **/
  case class IfThenElse[T:Staged](cond: Sym[Bool], thenp: Block[T], elsep: Block[T]) extends Op[T] {
    def mirror(f:Tx) = ifThenElse[T](f(cond), f(thenp), f(elsep))

    freqs   = normal(cond) ++ cold(thenp) ++ cold(elsep)
    aliases = List(thenp.getResult, elsep.getResult)
  }

  /** Smart Constructor **/
  def ifThenElse[T:Staged](cond: Sym[Bool], thenp: => Sym[T], elsep: => Sym[T])(implicit ctx: SrcCtx): Sym[T] = cond match {
    case Const(true) if context != null => thenp // Inlining is not valid if there is no outer context
    case Const(false) if context != null => elsep
    case Op(Not(x)) => ifThenElse(x, elsep, thenp)
    case _ =>
      val thenBlk = stageScope(thenp)
      val elseBlk = stageScope(elsep)
      val effects = thenBlk.summary orElse elseBlk.summary
      stageEffectful(IfThenElse(cond, thenBlk, elseBlk), effects)(ctx)
  }
}
