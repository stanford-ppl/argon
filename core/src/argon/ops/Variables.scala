package argon.ops

import argon._
import argon.core.Staging
import forge._

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

trait VariablesApi { this: ArgonApi =>

}

trait LowPriorityVariableImplicits { this: VariablesExp with Staging =>

  class FakeVarLift[T:Meta] extends Lift[Var[T],T] {
    override def apply(x: Var[T])(implicit ctx: SrcCtx): T = readVar(x)
  }
  implicit def createFakeVarLift[T:Meta]: Lift[Var[T],T] = new FakeVarLift[T]

}

trait VariablesExp extends LowPriorityVariableImplicits { this: ArgonExp =>

  case class Var[T:Meta](s: Exp[Var[T]])(implicit selfType: Meta[Var[T]]) extends MetaAny[Var[T]] {
    @api override def ===(that: Var[T]) = readVar(this) === readVar(that)
    @api override def =!=(that: Var[T]) = readVar(this) =!= readVar(that)
    @api override def toText = readVar(this).toText
  }

  case class VarType[T](child: Meta[T]) extends Meta[Var[T]] {
    override def wrapped(x: Exp[Var[T]]) = Var(x)(child,this)
    override def stagedClass = classOf[Var[T]]
    override def typeArguments = List(child)
    override def isPrimitive = false // Should not be primitive -- used to check aliases / mutable symbols
  }
  // NOTE: No implicit evidence of being staged

  import StagedVariables._
  @util def infix_==[T:Meta](lhs: Var[T], rhs: Var[T]): Bool = readVar(lhs) === readVar(rhs)
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): Bool = macro equalVarLeft[Bool]
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): Bool = macro equalVarRight[Bool]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalVarLiftRight[Bool]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): Bool = macro equalVarLiftLeft[Bool]

  @util def infix_!=[T:Meta](lhs: Var[T], rhs: Var[T]): Bool = readVar(lhs) =!= readVar(rhs)
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): Bool = macro unequalVarLeft[Bool]
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): Bool = macro unequalVarRight[Bool]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalVarLiftRight[Bool]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): Bool = macro unequalVarLiftLeft[Bool]

  @api implicit def readVar[T](v: Var[T]): T = {
    implicit val mT: Type[T] = v.s.tp.typeArguments.head.asInstanceOf[Type[T]]
    wrap(read_var(v.s))
  }

  @util def __newVar[T<:MetaAny[T]:Meta](init: MetaAny[T]): Var[T] = Var(new_var(init.s))(typ[T],VarType(typ[T]))
  @util def __newVar[A,T<:MetaAny[T]](init: A)(implicit lift: Lift[A,T]): Var[T] = {
    implicit val mT = lift.staged
    Var(new_var(lift(init).s))(typ[T],VarType(typ[T]))
  }

  @util def __readVar[T:Meta](v: Var[T]): T = readVar(v)

  @util def __assign[T<:MetaAny[T]:Meta](lhs: Var[T], rhs: T): Void = wrap(assign_var(lhs.s, rhs.s))
  @util def __assign[A,T<:MetaAny[T]](lhs: Var[T], rhs: A)(implicit lift: Lift[A,T]): Void = {
    implicit val mT = lift.staged
    wrap(assign_var(lhs.s, lift(rhs).s))
  }

  case class NewVar[T:Type](init: Exp[T])(implicit val tp: Type[Var[T]]) extends Op[Var[T]] {
    def mirror(f:Tx) = new_var(f(init))
    override def aliases = Nil
    override def contains = dyns(init)
    override def extracts = Nil
  }
  case class ReadVar[T:Type](v: Exp[Var[T]]) extends Op[T] {
    def mirror(f:Tx) = read_var(f(v))
    override def aliases = Nil
    override def contains = Nil
    override def extracts = dyns(v)
  }
  case class AssignVar[T:Type](v: Exp[Var[T]], x: Exp[T]) extends Op[Void] {
    def mirror(f:Tx) = assign_var(f(v),f(x))
    override def aliases = Nil
    override def contains = dyns(x)
    override def extracts = dyns(v)
  }

  @internal def new_var[T:Type](init: Exp[T])(implicit ctx: SrcCtx): Exp[Var[T]] = {
    implicit val tp: Type[Var[T]] = VarType(typ[T])
    stageMutable(NewVar(init))(ctx)
  }
  @internal def read_var[T:Type](v: Exp[Var[T]])(implicit ctx: SrcCtx): Exp[T] = stage(ReadVar(v))(ctx)
  @internal def assign_var[T:Type](v: Exp[Var[T]], x: Exp[T])(implicit ctx: SrcCtx): Exp[Void] = stageWrite(v)(AssignVar(v, x))(ctx)
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