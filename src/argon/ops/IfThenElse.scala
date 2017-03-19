package argon.ops
import argon.core.Staging

trait IfThenElseApi extends IfThenElseExp with BoolApi {
  this: TextApi =>
}

trait IfThenElseExp extends Staging with BoolExp with VoidExp {
  this: TextExp =>

  /** Virtualized Methods **/
  def __ifThenElse[A,B,C](cond: Bool, thenp: => A, elsep: => B)(implicit ctx: SrcCtx, lA: Lift[A,C], lB: Lift[B,C]): C = {
    implicit val staged: Staged[C] = lA.staged
    val unwrapThen = () => lA.staged.unwrapped( lA.lift(thenp) ) // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => lB.staged.unwrapped( lB.lift(elsep) )  // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  /** IR Nodes **/
  case class IfThenElse[T:Staged](cond: Exp[Bool], thenp: Block[T], elsep: Block[T]) extends Op[T] {
    def mirror(f:Tx) = ifThenElse[T](f(cond), f(thenp), f(elsep))

    override def freqs   = normal(cond) ++ cold(thenp) ++ cold(elsep)
    override def aliases = syms(thenp.result, elsep.result)
  }

  /** Constructors **/
  def ifThenElse[T:Staged](cond: Exp[Bool], thenp: => Exp[T], elsep: => Exp[T])(implicit ctx: SrcCtx): Exp[T] = cond match {
    case Const(true) if context != null => thenp // Inlining is not valid if there is no outer context
    case Const(false) if context != null => elsep
    case Op(Not(x)) => ifThenElse(x, elsep, thenp)
    case _ =>
      val thenBlk = stageBlock(thenp)
      val elseBlk = stageBlock(elsep)
      val effects = thenBlk.summary orElse elseBlk.summary
      stageEffectful(IfThenElse(cond, thenBlk, elseBlk), effects)(ctx)
  }
}
