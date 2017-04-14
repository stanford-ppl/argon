package argon

import argon.core._
import argon.ops._
import argon.typeclasses._


trait ArgonExp extends Staging
  with ArrayExp with ArrayExtExp with BoolExp with CastExp with FixPtExp with FltPtExp
  with HashMapExp with IfThenElseExp with StructExp
  with TextExp with TupleExp with VoidExp
  with BitsExp with CustomBitWidths with ArithExp with OrderExp
  with NumExp with Reporting

trait ArgonApi extends ArgonExp
  with ArrayApi with ArrayExtApi with BoolApi with CastApi with FixPtApi with FltPtApi
  with HashMapApi with IfThenElseApi with StructApi
  with TextApi with TupleApi with VoidApi
  with BitsApi with ArithApi with OrderApi
  with NumApi with LowPriorityNumImplicits