package argon.lang.direct

import argon.core._
import forge._
import virtualized.EmbeddedControls

trait IfThenElseExp extends EmbeddedControls {
  @internal def __ifThenElse[A, B, T<:MetaAny[T]](cond: MBoolean, thenp: => A, elsep: => B)(implicit liftA: Lift[A,T], liftB: Lift[B,T]): T = {
    implicit val staged: Type[T] = liftB.staged
    val unwrapThen = () => liftA(thenp).s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => liftB(elsep).s // wrapping it as a Function0 allows it to be delayed
    wrap(IfThenElseOps.ifThenElse[T](cond.s, unwrapThen, unwrapElse))
  }

  @internal def __ifThenElse[A, T<:MetaAny[T]](cond: MBoolean, thenp: => A, elsep: => T)(implicit lift: Cast[A,T], x1: Overload1): T = {
    implicit val staged: Type[T] = lift.staged
    val unwrapThen = () => lift(thenp).s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(IfThenElseOps.ifThenElse(cond.s, unwrapThen, unwrapElse))
  }

  @internal def __ifThenElse[A, T<:MetaAny[T]](cond: MBoolean, thenp: => T, elsep: => A)(implicit lift: Cast[A,T], x2: Overload2): T = {
    implicit val staged: Type[T] = lift.staged
    val unwrapThen = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => lift(elsep).s // wrapping it as a Function0 allows it to be delayed
    wrap(IfThenElseOps.ifThenElse(cond.s, unwrapThen, unwrapElse))
  }

  @internal def __ifThenElse[T<:MetaAny[T]:Type](cond: MBoolean, thenp: => T, elsep: => T): T = {
    val unwrapThen = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(IfThenElseOps.ifThenElse(cond.s, unwrapThen, unwrapElse))
  }

  @internal def ifThenElse[T:Type](cond: MBoolean, thenp: => T, elsep: => T): T = {
    val unwrapThen = () => thenp.s // directly calling unwrap(thenp) forces thenp to be evaluated here
    val unwrapElse = () => elsep.s // wrapping it as a Function0 allows it to be delayed
    wrap(IfThenElseOps.ifThenElse(cond.s, unwrapThen, unwrapElse))
  }
}

