package argon

import argon.lang._

trait ArgonInternal extends ArgonExp with ArgonCommon {
  /**
    * Convention (adapted from Forge):
    * M- prefix: "Meta" (staged types)
    * C- prefix: "Constant" (unstaged types)
    */

  type MetaAny[T] = argon.lang.MetaAny[T]

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

  type Func1[A,R] = argon.lang.Func1[A,R]
  type Func2[A,B,R] = argon.lang.Func2[A,B,R]
  type Func3[A,B,C,R] = argon.lang.Func3[A,B,C,R]
  type Func4[A,B,C,D,R] = argon.lang.Func4[A,B,C,D,R]
  type Func5[A,B,C,D,E,R] = argon.lang.Func5[A,B,C,D,E,R]
  type Func6[A,B,C,D,E,F,R] = argon.lang.Func6[A,B,C,D,E,F,R]
  type Func7[A,B,C,D,E,F,G,R] = argon.lang.Func7[A,B,C,D,E,F,G,R]
  type Func8[A,B,C,D,E,F,G,H,R] = argon.lang.Func8[A,B,C,D,E,F,G,H,R]
  type Func9[A,B,C,D,E,F,G,H,I,R] = argon.lang.Func9[A,B,C,D,E,F,G,H,I,R]
  type Func10[A,B,C,D,E,F,G,H,I,J,R] = argon.lang.Func10[A,B,C,D,E,F,G,H,I,J,R]
  val Func = argon.lang.Func
}

