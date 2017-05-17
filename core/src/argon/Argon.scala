package argon

import argon.core._
import argon.lang._
import argon.typeclasses._

trait ArgonCore
  extends Reporting
  with StagedTypes
  with BlocksCore
  with DefsCore
  with ExpsCore
  with EffectsCore
  with Lattices
  with MetadataCore
  with Scheduling
  with Staging
  with Statements

trait ArgonExp
  extends ArrayExp
  with BooleanExp
  with CastsExp
  with FixPtExp
  with FltPtExp
  with FunctionExp
  with HashMapExp
  with IfThenElseExp
  with StringExp
  with Tuple2Exp
  with UnitExp
  with ArithExp
  with BitsExp
  with NumExp
  with OrderExp

trait ArgonApi
  extends ArgonExp
  with CastsApi
  with FixPtApi
  with FltPtApi
  with FunctionApi
  with HashMapApi
  with IfThenElseApi
  with NumApi
