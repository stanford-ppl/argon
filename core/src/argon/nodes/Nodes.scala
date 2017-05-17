package argon.nodes

import argon.lang._

trait Nodes extends ArrayExp
  with BooleanExp
  with FixPtExp
  with FltPtExp
  with HashMapExp
  with StringExp
  with Tuple2Exp
  with UnitExp
  with VarExp
{
  /**
    * Convention (adapted from Forge):
    * M- prefix: "Meta" (staged types)
    * C- prefix: "Constant" (unstaged types)
    */
  type MArray[T] = argon.lang.Array[T]
  val MArray = argon.lang.Array
  type CArray[T] = scala.Array[T]

  type MHashMap[K, V] = argon.lang.HashMap[K, V]
  val MHashMap = argon.lang.HashMap

  type HashIndex[K] = argon.lang.HashIndex[K]

  type MBoolean = argon.lang.Boolean
  val MBoolean = argon.lang.Boolean
  type CBoolean = scala.Boolean

  type FixPt[S, I, F] = argon.lang.FixPt[S, I, F]
  val FixPt = argon.lang.FixPt

  type FltPt[G, E] = argon.lang.FltPt[G, E]
  val FltPt = argon.lang.FltPt

  type MString = argon.lang.String
  val MString = argon.lang.String
  type CString = java.lang.String

  type MTuple2[A, B] = argon.lang.Tuple2[A, B]
  val MTuple2 = argon.lang.Tuple2
  type CTuple2[A, B] = scala.Tuple2[A, B]

  type Struct[S] = argon.lang.Struct[S]
  val Struct = argon.lang.Struct

  type MUnit = argon.lang.Unit
  val MUnit = argon.lang.Unit
  type CUnit = scala.Unit

  type Var[T] = argon.lang.Var[T]
  val Var = argon.lang.Var
}