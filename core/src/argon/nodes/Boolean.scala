package argon.nodes

import argon._
import argon.typeclasses._
import forge._

case object BooleanType extends Type[MBoolean] with CanBits[MBoolean] {
  override def wrapped(x: Exp[MBoolean]): MBoolean = MBoolean(x)
  override def typeArguments = Nil
  override def stagedClass = classOf[MBoolean]
  override def isPrimitive = true
  protected def getBits(children: Seq[Type[_]]) = Some(BooleanBits)

  def unapply(x: Type[_]): Boolean = x == BooleanType
}
object BooleanBits extends Bits[MBoolean] {
  @internal def zero: MBoolean = MBoolean(false)
  @internal def one: MBoolean = MBoolean(true)
  @internal def random(max: Option[MBoolean]): MBoolean = MBoolean(MBoolean.random(max.map(_.s)))
  @internal def length = 1
}

/** IR Nodes **/
sealed abstract class MBooleanOp extends Op[MBoolean] { protected val bool = MBoolean }

case class Not (a: Exp[MBoolean]) extends MBooleanOp { def mirror(f:Tx) = bool.not(f(a)) }
case class And (a: Exp[MBoolean], b: Exp[MBoolean]) extends MBooleanOp { def mirror(f:Tx) = bool.and(f(a), f(b)) }
case class Or  (a: Exp[MBoolean], b: Exp[MBoolean]) extends MBooleanOp { def mirror(f:Tx) = bool.or(f(a), f(b)) }
case class XOr (a: Exp[MBoolean], b: Exp[MBoolean]) extends MBooleanOp { def mirror(f:Tx) = bool.xor(f(a), f(b)) }
case class XNor(a: Exp[MBoolean], b: Exp[MBoolean]) extends MBooleanOp { def mirror(f:Tx) = bool.xnor(f(a), f(b)) }
case class RandomBoolean(max: Option[Exp[MBoolean]]) extends MBooleanOp { def mirror(f:Tx) = bool.random(f(max)) }
case class StringToBoolean(x: Exp[MString]) extends MBooleanOp { def mirror(f:Tx) = bool.from_string(x) }


