package argon.lang.direct

import argon.core._
import forge._

trait BooleanExp {
  @api implicit def boolean2bool(x: CBoolean): MBoolean = MBoolean(x)

  /** Lifting **/
  implicit object LiftBoolean2Bool extends Lift[CBoolean,MBoolean] {
    @internal def apply(x: CBoolean): MBoolean = MBoolean(x)
  }
  /** Casting **/
  implicit object CastBoolean2Bool extends Cast[CBoolean,MBoolean] {
    @internal def apply(x: CBoolean): MBoolean = MBoolean(x)
  }
  implicit object String2Boolean extends Cast[MString,MBoolean] {
    @internal def apply(x: MString): MBoolean = MBoolean(MBoolean.from_string(x.s))
  }
}
