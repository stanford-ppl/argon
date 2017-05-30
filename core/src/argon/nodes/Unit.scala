package argon.nodes

import argon.core.compiler._
import argon.compiler._

object UnitType extends Type[MUnit] {
  override def wrapped(x: Exp[MUnit]) = MUnit(x)
  override def stagedClass = classOf[MUnit]
  override def isPrimitive = true
}
