package argon.ops
import argon.core.Staging
import forge._

trait IfThenElseApi extends IfThenElseExp with BoolApi {
  this: TextApi =>
}

trait IfThenElseExp extends Staging with BoolExp with VoidExp with OverloadHack {
  this: TextExp =>

  /** Virtualized Methods **/
  @util def __ifThenElse[A, B, T<:MetaAny[T]](cond: Bool, thenp: => A, elsep: => B)(implicit liftA: Lift[A,T], liftB: Lift[B,T]): T = {
    implicit val staged: Meta[T] = liftB.staged
    val unwrapThen = () => liftA(thenp).s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => liftB(elsep).s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse[T](cond.s, unwrapThen(), unwrapElse()))
  }

  @util def __ifThenElse[A, T<:MetaAny[T]](cond: Bool, thenp: => A, elsep: => T)(implicit lift: Cast[A, T], x1: Overload1): T = {
    implicit val staged: Meta[T] = lift.staged
    val unwrapThen = () => lift(thenp).s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  @util def __ifThenElse[A, T<:MetaAny[T]](cond: Bool, thenp: => T, elsep: => A)(implicit lift: Cast[A,T], x2: Overload2): T = {
    implicit val staged: Meta[T] = lift.staged
    val unwrapThen = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => lift(elsep).s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  @util def __ifThenElse[T<:MetaAny[T]:Meta](cond: Bool, thenp: => T, elsep: => T): T = {
    val unwrapThen: () => Exp[T] = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse: () => Exp[T] = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen(), unwrapElse()))
  }

  // special hack for "if (x) { thing }"
  // TODO: Still needed?
  /*def __ifThenElse(cond: Bool, thenp: => Void, elsep: => Unit)(implicit ctx: SrcCtx): Void = {
    val elseBlk = () => lift(elsep)
    __ifThenElse(cond, thenp, elseBlk())
  }*/

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
