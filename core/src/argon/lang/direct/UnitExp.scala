package argon.lang.direct

import argon.core._
import forge._

trait UnitExp {
  @api implicit def liftUnit(x: Unit): MUnit = MUnit()

  /** Lifting **/
  implicit object UnitLift extends Lift[Unit,MUnit] {
    @internal override def apply(x: Unit) = MUnit()
  }

  /** Casting **/
  implicit object UnitCast extends Cast[Unit,MUnit] {
    @internal override def apply(x: Unit) = MUnit()
  }
}
