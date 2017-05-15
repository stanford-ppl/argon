package argon.lang

import argon._
import forge._

import scala.reflect.macros.whitebox

case class Var[T:Type](s: Exp[Var[T]])(implicit selfType: Type[Var[T]]) extends MetaAny[Var[T]] {
  protected def read = Var.read_var(this.s)

  @api override def ===(that: Var[T]) = this.read === that.read
  @api override def =!=(that: Var[T]) = this.read =!= that.read
  @api override def toText = this.read.toText
}

case class VarType[T](child: Type[T]) extends Type[Var[T]] {
  override def wrapped(x: Exp[Var[T]]) = Var(x)(child,this)
  override def stagedClass = classOf[Var[T]]
  override def typeArguments = List(child)
  override def isPrimitive = false // Should NOT be primitive -- used to check aliases / mutable symbols
}
// NOTE: NO implicit evidence of being staged (otherwise could have something like Array[Var[T]])

object Var {
  @internal def new_var[T:Type](init: Exp[T]): Exp[Var[T]] = {
    implicit val tp: Type[Var[T]] = VarType(typ[T])
    stageMutable(NewVar(init))(ctx)
  }
  @internal def read_var[T:Type](v: Exp[Var[T]]): Exp[T] = stage(ReadVar(v))(ctx)
  @internal def assign_var[T:Type](v: Exp[Var[T]], x: Exp[T]): Exp[Void] = stageWrite(v)(AssignVar(v, x))(ctx)
}

trait LowPriorityVarImplicits { this: VarExp =>

  class FakeVarLift[T:Type] extends Lift[Var[T],T] {
    @internal def apply(x: Var[T]): T = readVar(x)
  }
  implicit def createFakeVarLift[T:Type]: Lift[Var[T],T] = new FakeVarLift[T]

}


trait VarExp extends LowPriorityVarImplicits {
  /** Static methods **/
  import StagedVariables._
  @internal def infix_==[T:Type](lhs: Var[T], rhs: Var[T]): Bool = readVar(lhs) === readVar(rhs)
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): Bool = macro equalVarLeft[Bool]
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): Bool = macro equalVarRight[Bool]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalVarLiftRight[Bool]
  def infix_==[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): Bool = macro equalVarLiftLeft[Bool]

  @internal def infix_!=[T:Type](lhs: Var[T], rhs: Var[T]): Bool = readVar(lhs) =!= readVar(rhs)
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: Var[A], x2: B): Bool = macro unequalVarLeft[Bool]
  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: Var[B]): Bool = macro unequalVarRight[Bool]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: Var[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalVarLiftRight[Bool]
  def infix_!=[A, B<:MetaAny[B], C<:MetaAny[C]](x1: A, x2: Var[B])(implicit l: Lift[A,C]): Bool = macro unequalVarLiftLeft[Bool]

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

  @internal def __assign[T<:MetaAny[T]:Type](lhs: Var[T], rhs: T): Void = Void(Var.assign_var(lhs.s, rhs.s))
  @internal def __assign[A,T<:MetaAny[T]](lhs: Var[T], rhs: A)(implicit lift: Lift[A,T]): Void = {
    implicit val mT = lift.staged
    Void(Var.assign_var(lhs.s, lift(rhs).s))
  }
}


/** IR Nodes **/
case class NewVar[T:Type](init: Exp[T])(implicit val tp: Type[Var[T]]) extends Op[Var[T]] {
  def mirror(f:Tx) = Var.new_var(f(init))
  override def aliases = Nil
  override def contains = dyns(init)
  override def extracts = Nil
}
case class ReadVar[T:Type](v: Exp[Var[T]]) extends Op[T] {
  def mirror(f:Tx) = Var.read_var(f(v))
  override def aliases = Nil
  override def contains = Nil
  override def extracts = dyns(v)
}
case class AssignVar[T:Type](v: Exp[Var[T]], x: Exp[T]) extends Op[Void] {
  def mirror(f:Tx) = Var.assign_var(f(v),f(x))
  override def aliases = Nil
  override def contains = dyns(x)
  override def extracts = dyns(v)
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
