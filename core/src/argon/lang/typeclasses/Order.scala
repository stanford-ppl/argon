package argon.lang.typeclasses

import argon.lang.MBoolean
import forge._

/** Staged types with ordering **/
trait Order[T] {
  @api def lessThan(a: T, b: T): MBoolean
  @api def lessThanOrEqual(a: T, b: T): MBoolean
  @api def equal(a: T, b: T): MBoolean
}
