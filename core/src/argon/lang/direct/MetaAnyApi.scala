package argon.lang.direct

import argon.core._
import forge._
import virtualized.EmbeddedControls

trait MetaAnyLowPriorityImplicits {
  // Has to be an implicit class to not conflict with higher priority implicits on +
  implicit class ConcatOps(lhs: MetaAny[_]) {
    @api def +(rhs: CString): MString = lhs.toText + MString(rhs)
    @api def +(rhs: MString): MString = lhs.toText + rhs
    //@api def +(rhs: MetaAny[_]): MString = lhs.toText + rhs.toText
  }
}

trait MetaAnyExp extends EmbeddedControls with MetaAnyLowPriorityImplicits {

  @internal def infix_toString(x: MetaAny[_]): MString = x.toText

  def __valDef[T<:MetaAny[T]](init: T, name: CString): CUnit = { init.s.name = Some(name) }

  // TODO: Should casts be implicit or explicit? Should have subtypes?
  @internal def __equals[T<:MetaAny[T]](x: T, y: T): MBoolean = x === y
  @internal def __equals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): MBoolean = lift(x) === y
  @internal def __equals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): MBoolean = x === lift(y)
  @internal def __unequals[T<:MetaAny[T]](x: T, y: T): MBoolean = x =!= y
  @internal def __unequals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): MBoolean = lift(x) =!= y
  @internal def __unequals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): MBoolean = x =!= lift(y)
}

