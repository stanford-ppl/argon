package argon.lang.direct

import argon.core._
import forge._

import scala.collection.immutable.{StringOps, WrappedString}

// Side note, I can't believe this actually works
trait StringSuperLowImplicits {
  implicit def wrapStringUnstaged(x: CString): WrappedString = new WrappedString(x)
}

trait StringVeryLowImplicits extends StringSuperLowImplicits {
  implicit def augmentStringUnstaged(x: CString): StringOps = new StringOps(x)
}

trait StringLowPriorityImplicits extends StringVeryLowImplicits {
  // Shadows Predef method..
  @api implicit def wrapString(x: CString): MString = MString(x)
}

trait StringExp extends StringLowPriorityImplicits {
  /** Static methods **/
  @internal def infix_+[R<:MetaAny[R]](x1: CString, x2: R): MString = MString(x1) + x2.toText

  @internal def char(x: Int8): MString = MString.char(x)

  // Shadows Predef method...
  @api implicit def augmentString(x: CString): MString = MString(x)

  /** Lifting **/
  implicit object LiftString extends Lift[CString,MString] {
    @internal def apply(x: CString): MString = MString(x)
  }

  /** Casting **/
  implicit object CastStringLift extends Cast[CString,MString] {
    @internal def apply(x: CString): MString = MString(x)
  }
  implicit object CastMStringLift extends Cast[MString,MString] {
    @internal def apply(x: MString): MString = x
  }
}
