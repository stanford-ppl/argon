package argon.lang.direct

import argon.core._
import argon.nodes._
import forge._

import scala.reflect.macros.whitebox
import scala.language.experimental.macros


trait LowPriorityVarImplicits { self: VarExp =>
  class FakeVarLift[T:Type] extends Lift[Var[T],T] {
    @internal def apply(x: Var[T]): T = readVar(x)
  }
  implicit def createFakeVarLift[T:Type]: Lift[Var[T],T] = new FakeVarLift[T]
}


trait VarExp extends LowPriorityVarImplicits {
  // NOTE: NO implicit evidence of being staged (otherwise could have something like Array[Var[T]])

  /** Static methods **/
  import StagedVariables._
  @internal def infix_==[T:Type](lhs: Var[T], rhs: Var[T]): MBoolean = readVar(lhs) === readVar(rhs)
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): MBoolean = macro equalVarLeft[MBoolean]
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): MBoolean = macro equalVarRight[MBoolean]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro equalVarLiftRight[MBoolean]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): MBoolean = macro equalVarLiftLeft[MBoolean]

  @internal def infix_!=[T:Type](lhs: Var[T], rhs: Var[T]): MBoolean = readVar(lhs) =!= readVar(rhs)
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): MBoolean = macro unequalVarLeft[MBoolean]
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): MBoolean = macro unequalVarRight[MBoolean]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): MBoolean = macro unequalVarLiftRight[MBoolean]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): MBoolean = macro unequalVarLiftLeft[MBoolean]

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

private object StagedVariables {
  def equalVarLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) === $x2")
  }
  def equalVarRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"x1 === readVar($x2)")
  }

  def equalVarLiftRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) === lift($x2)")
  }

  def equalVarLiftLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === readVar($x2)")
  }

  def unequalVarLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) =!= $x2")
  }

  def unequalVarRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 =!= readVar($x2)")
  }

  def unequalVarLiftRight[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"readVar($x1) =!= lift($x2)")
  }

  def unequalVarLiftLeft[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) =!= readVar($x2)")
  }
}
