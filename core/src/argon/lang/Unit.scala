package argon.lang

import argon._
import argon.nodes._
import forge._

case class Unit(s: Exp[Unit]) extends MetaAny[Unit] {
  override type Internal = scala.Unit
  @api def ===(that: Unit): MBoolean = true
  @api def =!=(that: Unit): MBoolean = false
  @api def toText: MString = String.ify(this)
}

object Unit {
  @internal def apply(): MUnit = MUnit(const())
  @internal def apply(s: CUnit) = MUnit(const())
  @internal def const(): Exp[MUnit] = constant[MUnit](())
}

trait UnitExp {
  implicit val voidIsStaged: Type[Unit] = UnitType

  /** Lifting **/
  @api implicit def unit2void(x: Unit): Unit = Unit()

  implicit object LiftUnit2Unit extends Lift[Unit,Unit] {
    @internal override def apply(x: Unit) = Unit()
  }

  /** Casting **/
  implicit object CastUnit2Unit extends Cast[Unit,Unit] {
    @internal override def apply(x: Unit) = Unit()
  }
}
