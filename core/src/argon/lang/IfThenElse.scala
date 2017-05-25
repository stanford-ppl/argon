package argon.lang

import argon.core.compiler._
import argon.nodes._
import forge._

object IfThenElseOps {
  /** Constructors **/
  @internal def ifThenElse[T:Type](cond: Exp[MBoolean], thenp: () => Exp[T], elsep: () => Exp[T]): Exp[T] = cond match {
    case Const(true) if state.context != null => thenp() // Inlining is not valid if there is no outer context
    case Const(false) if state.context != null => elsep()
    case Op(Not(x)) => ifThenElse(x, elsep, thenp)
    case _ =>
      val thenBlk = stageColdBlock(thenp())
      val elseBlk = stageColdBlock(elsep())
      val effects = thenBlk.effects orElse elseBlk.effects
      stageEffectful(IfThenElse(cond, thenBlk, elseBlk), effects)(ctx)
  }
}

trait IfThenElseExp {
  import IfThenElseOps._

  @internal def __ifThenElse[A, B, T<:MetaAny[T]](cond: MBoolean, thenp: => A, elsep: => B)(implicit liftA: Lift[A,T], liftB: Lift[B,T]): T = {
    implicit val staged: Type[T] = liftB.staged
    val unwrapThen = () => liftA(thenp).s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => liftB(elsep).s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse[T](cond.s, unwrapThen, unwrapElse))
  }

  @internal def __ifThenElse[A, T<:MetaAny[T]](cond: MBoolean, thenp: => A, elsep: => T)(implicit lift: Cast[A,T], x1: Overload1): T = {
    implicit val staged: Type[T] = lift.staged
    val unwrapThen = () => lift(thenp).s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen, unwrapElse))
  }

  @internal def __ifThenElse[A, T<:MetaAny[T]](cond: MBoolean, thenp: => T, elsep: => A)(implicit lift: Cast[A,T], x2: Overload2): T = {
    implicit val staged: Type[T] = lift.staged
    val unwrapThen = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => lift(elsep).s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen, unwrapElse))
  }

  @internal def __ifThenElse[T<:MetaAny[T]:Type](cond: MBoolean, thenp: => T, elsep: => T): T = {
    val unwrapThen = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(ifThenElse(cond.s, unwrapThen, unwrapElse))
  }
}

trait IfThenElseApi {

}


