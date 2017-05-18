package argon

import argon.lang.ArgonApi
import forge._

trait ArgonExternal extends ArgonApi with ArgonCommon {
  type Type[T] = argon.Type[T]
  type Exp[T] = argon.Exp[T]

  type MetaAny[T] = argon.lang.MetaAny[T]
  type Any = argon.lang.MetaAny[_]

  type Array[T] = argon.lang.Array[T]
  val Array = argon.lang.Array

  type HashMap[K, V] = argon.lang.HashMap[K, V]
  val HashMap = argon.lang.HashMap

  type Boolean = argon.lang.Boolean
  val Boolean = argon.lang.Boolean

  type FixPt[S, I, F] = argon.lang.FixPt[S, I, F]
  val FixPt = argon.lang.FixPt

  type FltPt[G, E] = argon.lang.FltPt[G, E]
  val FltPt = argon.lang.FltPt

  type String = argon.lang.String
  val String = argon.lang.String

  type Tuple2[A, B] = argon.lang.Tuple2[A, B]
  val Tuple2 = argon.lang.Tuple2

  type Struct[S] = argon.lang.Struct[S]

  type Unit = argon.lang.Unit

  type Var[T] = argon.lang.Var[T]

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
}
