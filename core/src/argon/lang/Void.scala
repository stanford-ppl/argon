package argon.lang

import argon._
import forge._

case class Void(s: Exp[Void]) extends MetaAny[Void] {
  @api def ===(that: Void): Bool = true
  @api def =!=(that: Void): Bool = false
  @api def toText: Text = Text.ify(this)
}

object VoidType extends Type[Void] {
  override def wrapped(x: Exp[Void]) = Void(x)
  override def stagedClass = classOf[Void]
  override def isPrimitive = true
}

object Void {
  @internal def apply(): Void = Void(const())
  @internal def apply(s: Unit) = Void(const())
  @internal def const(): Exp[Void] = constant[Void](())
}

trait VoidExp {
  implicit val voidIsStaged: Type[Void] = VoidType

  /** Lifting **/
  @api implicit def unit2void(x: Unit): Void = Void()

  implicit object LiftUnit2Void extends Lift[Unit,Void] {
    @internal override def apply(x: Unit) = Void()
  }

  /** Casting **/
  implicit object CastUnit2Void extends Cast[Unit,Void] {
    @internal override def apply(x: Unit) = Void()
  }
}

trait VoidApi {
  type Unit = Void
}