package argon.lang.direct

import argon.core._
import argon.nodes._
import forge._


trait LowPriorityVarImplicits { self: VarExp =>
  class FakeVarLift[T:Type] extends Lift[Var[T],T] {
    @internal def apply(x: Var[T]): T = readVar(x)
  }
  implicit def createFakeVarLift[T:Type]: Lift[Var[T],T] = new FakeVarLift[T]
}


trait VarExp extends LowPriorityVarImplicits {
  // NOTE: NO implicit evidence of being staged (otherwise could have something like Array[Var[T]])

  // TODO: How to virtualize these assignment operators in general?
  // Should we have a general rewrite rule for x [op]= y ==> x = readVar(X) [op] y?
  // Should be lower priority than readVar(x) [op]= y ?
  implicit class VarArithOps[T:Arith:Type](lhs: Var[T]) {
    @api def +=(rhs: T): MUnit = wrap(Var.assign_var(lhs.s, implicitly[Arith[T]].plus(readVar(lhs), rhs).s))
    @api def -=(rhs: T): MUnit = wrap(Var.assign_var(lhs.s, implicitly[Arith[T]].minus(readVar(lhs), rhs).s))
    @api def *=(rhs: T): MUnit = wrap(Var.assign_var(lhs.s, implicitly[Arith[T]].times(readVar(lhs), rhs).s))
    @api def /=(rhs: T): MUnit = wrap(Var.assign_var(lhs.s, implicitly[Arith[T]].divide(readVar(lhs), rhs).s))
  }

  /** Static methods **/
  @internal def infix_==[T:Type](lhs: Var[T], rhs: Var[T]): MBoolean = readVar(lhs) === readVar(rhs)

  @internal def infix_!=[T:Type](lhs: Var[T], rhs: Var[T]): MBoolean = readVar(lhs) =!= readVar(rhs)

  @api implicit def readVar[T](v: Var[T]): T = {
    implicit val mT: Type[T] = v.s.tp.typeArguments.head.asInstanceOf[Type[T]]
    wrap(Var.read_var(v.s))
  }

  @internal def __newVar[T<:MetaAny[T]:Type](init: MetaAny[T]): Var[T] = Var(Var.new_var(init.s))(typ[T],VarType(typ[T]))
  @internal def __newVar[A,T<:MetaAny[T]](init: A)(implicit lift: Lift[A,T]): Var[T] = {
    implicit val mT = lift.staged
    Var(Var.new_var(lift(init).s))(typ[T],VarType(typ[T]))
  }

  @internal def __readVar[T:Type](v: Var[T]): T = readVar(v)

  @internal def __assign[T<:MetaAny[T]:Type](lhs: Var[T], rhs: T): MUnit = MUnit(Var.assign_var(lhs.s, rhs.s))
  @internal def __assign[A,T<:MetaAny[T]](lhs: Var[T], rhs: A)(implicit lift: Lift[A,T]): MUnit = {
    implicit val mT = lift.staged
    MUnit(Var.assign_var(lhs.s, lift(rhs).s))
  }
}
