package argon.lang

import argon.core._
import argon.nodes._
import forge._

case class Unit(s: Exp[Unit]) extends MetaAny[Unit] {
  override type Internal = scala.Unit
  @api def ===(that: Unit): MBoolean = true
  @api def =!=(that: Unit): MBoolean = false
  @api def toText: MString = String.ify(this)
}

object Unit {
  implicit def unitIsStaged: Type[MUnit] = UnitType

  @internal def apply(): MUnit = Unit(const())
  @internal def apply(s: CUnit): MUnit = Unit(const())
  @internal def const(): Exp[MUnit] = constant(UnitType)(())
}

