package argon.lang.typeclasses

import argon.lang.MBoolean
import forge._

/** Staged types with ordering **/
trait Order[T] {
  /** Returns true if `a` is less than `b`, false otherwise. **/
  @api def lessThan(a: T, b: T): MBoolean

  /** Returns true if `a` is less than or equal to `b`, false otherwise. **/
  @api def lessThanOrEqual(a: T, b: T): MBoolean

  /** Returns true if `a` is equal to `b`, false otherwise. **/
  @api def equal(a: T, b: T): MBoolean
}
