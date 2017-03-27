package argon.ops
import argon.core.Staging

trait IfThenElseApi extends IfThenElseExp with BoolApi {
  this: TextApi =>
}

//hack around https://github.com/scalameta/scalameta/issues/667
object Fun0{
  def apply[T](e:T): () => T = () => e
}

trait IfThenElseExp extends Staging with BoolExp with VoidExp {
  this: TextExp =>

  /** Virtualized Methods **/
  def __ifThenElse[A, B, T](cond: Bool, thenp: => A, elsep: => B)(implicit ctx: SrcCtx, liftA: Lift[A,T], liftB: Lift[B,T]): T = {
    implicit val staged: Meta[T] = liftB.staged
    val unwrapThen = Fun0(liftA(thenp).s) // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = Fun0(liftB(elsep).s) // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse[T](cond.s, unwrapThen(), unwrapElse()))
  }

  def __ifThenElse[A, T:Meta](cond: Bool, thenp: => A, elsep: => T)(implicit ctx: SrcCtx, lift: Lift[A, T]): T = {
    val unwrapThen = Fun0(lift(thenp).s) // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = Fun0(elsep.s) // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  def __ifThenElse[A, T:Meta](cond: Bool, thenp: => T, elsep: => A)(implicit ctx: SrcCtx, lift: Lift[A,T]): T = {
    val unwrapThen = Fun0(thenp.s) // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = Fun0(lift(elsep).s) // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  def __ifThenElse[T:Meta](cond: Bool, thenp: => T, elsep: => T)(implicit ctx: SrcCtx): T = {
    val unwrapThen: () => Exp[T] = Fun0(thenp.s) // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse: () => Exp[T] = Fun0(elsep.s) // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  // special hack for "if (x) { thing }"
  // TODO: Still needed?
  def __ifThenElse(cond: Bool, thenp: => Void, elsep: => Unit)(implicit ctx: SrcCtx): Void = {
    val elseBlk = Fun0(lift(elsep))
    __ifThenElse(cond, thenp, elseBlk())
  }

  /** IR Nodes **/
  case class IfThenElse[T:Type](cond: Exp[Bool], thenp: Block[T], elsep: Block[T]) extends Op[T] {
    def mirror(f:Tx) = ifThenElse[T](f(cond), f(thenp), f(elsep))

    override def freqs   = normal(cond) ++ cold(thenp) ++ cold(elsep)
    override def aliases = dyns(thenp.result, elsep.result)
  }

  /** Constructors **/
  def ifThenElse[T:Type](cond: Exp[Bool], thenp: => Exp[T], elsep: => Exp[T])(implicit ctx: SrcCtx): Exp[T] = cond match {
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
