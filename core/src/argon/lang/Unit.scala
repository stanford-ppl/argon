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
  @internal def apply(s: CUnit): MUnit = MUnit(const())
  @internal def const(): Exp[MUnit] = constant(UnitType)(())
}

trait UnitExp {
  implicit val voidIsStaged: Type[MUnit] = UnitType

  /** Lifting **/
  @api implicit def unit2void(x: CUnit): MUnit = MUnit()

  implicit object LiftUnit2Unit extends Lift[CUnit,MUnit] {
    @internal override def apply(x: CUnit) = MUnit()
  }

  /** Casting **/
  implicit object CastUnit2Unit extends Cast[CUnit,MUnit] {
    @internal override def apply(x: CUnit) = MUnit()
  }
}
