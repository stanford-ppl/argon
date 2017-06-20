package argon.nodes

import argon.core._
import argon.compiler._

case class VarType[T](child: Type[T]) extends Type[Var[T]] {
  override def wrapped(x: Exp[Var[T]]) = Var(x)(child,this)
  override def stagedClass = classOf[Var[T]]
  override def typeArguments = List(child)
  override def isPrimitive = false // Should NOT be primitive -- used to check aliases / mutable symbols
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
case class AssignVar[T:Type](v: Exp[Var[T]], x: Exp[T]) extends Op[MUnit] {
  def mirror(f:Tx) = Var.assign_var(f(v),f(x))
  override def aliases = Nil
  override def contains = dyns(x)
  override def extracts = dyns(v)
}

