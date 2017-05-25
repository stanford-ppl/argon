package argon.nodes

import argon.core.compiler._
import argon.lang.compiler._

object StringType extends Type[MString] {
  def wrapped(x: Exp[MString]) = MString(x)
  def stagedClass = classOf[MString]
  def isPrimitive = true
}

/** IR Nodes **/
case class ToString[T:Type](x: Exp[T]) extends Op[MString] {
  def mirror(f:Tx) = MString.sym_tostring(f(x))
  val mT = typ[T]
}
case class StringConcat(x: Exp[MString], y: Exp[MString]) extends Op[MString]  { def mirror(f:Tx) = MString.concat(f(x),f(y)) }
case class StringEquals(x: Exp[MString], y: Exp[MString]) extends Op[MBoolean] { def mirror(f:Tx) = MString.equals(f(x),f(y)) }
case class StringDiffer(x: Exp[MString], y: Exp[MString]) extends Op[MBoolean] { def mirror(f:Tx) = MString.differ(f(x),f(y)) }
