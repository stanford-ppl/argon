package argon.nodes

import argon.core._
import argon.compiler._

case class FuncType[R](r: Type[R]) extends Type[Func[R]] {
  override def wrapped(x: Exp[Func[R]]) = new Func(x)(r)
  override def unwrapped(x: Func[R]) = x.s
  override def stagedClass = classOf[Func[R]]
  override def typeArguments = List(r)
  override def isPrimitive: CBoolean = false
}

case class FuncCall[R:Type](func: Exp[Func[R]], l: List[Exp[_]]) extends Op[R] {
  def mirror(f:Tx) = Func.call(f(func),f.tx(l))
  override def inputs = dyns(func) ++ dyns(l)
  override def aliases = Nil
}

case class FuncDecl[R:Type](l: List[Bound[_]], block: Block[R]) extends Op[Func[R]] {
  def mirror(f:Tx) = Func.decl(l, f(block))
  override def inputs  = dyns(block)  // Don't include the bounds as inputs
  override def binds   = super.binds ++ l
  override def aliases = Nil
  val mRet: Type[R] = typ[R]
}
