package argon.lang.typeclasses

import argon.lang.{FixPt,FltPt}
import argon.core._
import argon.nodes._
import forge._

trait Num[T] extends Bits[T] with Arith[T] with Order[T] {
  @api def toFixPt[S:BOOL,I:INT,F:INT](x: T): FixPt[S,I,F]
  @api def toFltPt[G:INT,E:INT](x: T): FltPt[G,E]

  @api def fromInt(x: Int, force: Boolean = true): T
  @api def fromLong(x: Long, force: Boolean = true): T
  @api def fromFloat(x: Float, force: Boolean = true): T
  @api def fromDouble(x: Double, force: Boolean = true): T
}
