package argon.lang

import typeclasses._
import forge._

/** Internal, language type aliases (no cyclic aliases allowed, e.g. cannot have "type X = argon.lang.X") **/
trait ArgonLangAliases {
  /**
    * Convention (adapted from Forge):
    * M- prefix: "Meta" (staged types)
    * C- prefix: "Constant" (unstaged types)
    */
  type Index = FixPt[TRUE,_32,_0]
  type Int64 = FixPt[TRUE,_64,_0]
  type Int32 = FixPt[TRUE,_32,_0]
  type Int16 = FixPt[TRUE,_16,_0]
  type  Int8 = FixPt[TRUE, _8,_0]
  @generate
  type UIntJJ$JJ$2to128 = FixPt[FALSE,_JJ,_0]

  type Float64 = FltPt[_53,_11]
  type Float32 = FltPt[_24, _8]
  type Float16 = FltPt[_11, _5]

  type MArray[T] = argon.lang.Array[T]
  type CArray[T] = scala.Array[T]

  type MHashMap[K, V] = argon.lang.HashMap[K, V]

  type MBoolean = argon.lang.Boolean
  type CBoolean = scala.Boolean

  type MString = argon.lang.String
  type CString = java.lang.String

  type MTuple2[A,B] = argon.lang.Tuple2[A,B]
  type CTuple2[A,B] = scala.Tuple2[A,B]

  type MUnit = argon.lang.Unit
  type CUnit = scala.Unit
}
