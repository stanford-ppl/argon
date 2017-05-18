package argon.lang

import argon.typeclasses._

/** Implicit type evidence, (optionally) implicit conversions, internal type aliases **/
trait ArgonExp
  extends ArrayExp
    with AssertExp
    with BooleanExp
    with FixPtExp
    with FltPtExp
    with FunctionExp
    with HashMapExp
    with IfThenElseExp
    with MetaAnyExp
    with OverloadHackExp
    with PrintExp
    with StringExp
    with StructExp
    with Tuple2Exp
    with UnitExp
    with VarExp
    with ArithExp
    with CustomBitWidthsExp
    with BitsExp
    with NumExp
    with OrderExp

/** Static functions, implicit conversions, app-facing type aliases **/
trait ArgonApi
  extends ArgonExp
    with AssertApi
    with CastsApi
    with FixPtApi
    with FltPtApi
    with HashMapApi
    with IfThenElseApi
    with PrintApi
    with BitsApi
    with NumApi
    with ArithApi