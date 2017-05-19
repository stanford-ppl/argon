package argon.lang

import argon.lang.typeclasses._

/** Internal type aliases (no cyclic aliases) **/
trait CommonAliases {
  /**
    * Convention (adapted from Forge):
    * M- prefix: "Meta" (staged types)
    * C- prefix: "Constant" (unstaged types)
    */
  type Index = FixPt[TRUE,_32,_0]

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

/** All external-facing type aliases **/
trait ExternalAliases extends CommonAliases {
  type FixPt[S,I,F] = argon.lang.FixPt[S,I,F]
  type FltPt[G,E] = argon.lang.FltPt[G,E]

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

  type HashIndex[K] = argon.lang.HashIndex[K]

  type MetaAny[T] = argon.lang.MetaAny[T]

  type Struct[S] = argon.lang.Struct[S]

  type Var[T] = argon.lang.Var[T]

  type INT[T] = argon.lang.typeclasses.INT[T]
  type BOOL[T] = argon.lang.typeclasses.BOOL[T]

  type TRUE = argon.lang.typeclasses.TRUE
  type FALSE = argon.lang.typeclasses.FALSE

  type _0 = argon.lang.typeclasses._0
  type _1 = argon.lang.typeclasses._1
  type _2 = argon.lang.typeclasses._2
  type _3 = argon.lang.typeclasses._3
  type _4 = argon.lang.typeclasses._4
  type _5 = argon.lang.typeclasses._5
  type _6 = argon.lang.typeclasses._6
  type _7 = argon.lang.typeclasses._7
  type _8 = argon.lang.typeclasses._8
  type _9 = argon.lang.typeclasses._9
  type _10 = argon.lang.typeclasses._10
  type _11 = argon.lang.typeclasses._11
  type _12 = argon.lang.typeclasses._12
  type _13 = argon.lang.typeclasses._13
  type _14 = argon.lang.typeclasses._14
  type _15 = argon.lang.typeclasses._15
  type _16 = argon.lang.typeclasses._16
  type _17 = argon.lang.typeclasses._17
  type _18 = argon.lang.typeclasses._18
  type _19 = argon.lang.typeclasses._19
  type _20 = argon.lang.typeclasses._20
  type _21 = argon.lang.typeclasses._21
  type _22 = argon.lang.typeclasses._22
  type _23 = argon.lang.typeclasses._23
  type _24 = argon.lang.typeclasses._24
  type _25 = argon.lang.typeclasses._25
  type _26 = argon.lang.typeclasses._26
  type _27 = argon.lang.typeclasses._27
  type _28 = argon.lang.typeclasses._28
  type _29 = argon.lang.typeclasses._29
  type _30 = argon.lang.typeclasses._30
  type _31 = argon.lang.typeclasses._31
  type _32 = argon.lang.typeclasses._32
  type _33 = argon.lang.typeclasses._33
  type _34 = argon.lang.typeclasses._34
  type _35 = argon.lang.typeclasses._35
  type _36 = argon.lang.typeclasses._36
  type _37 = argon.lang.typeclasses._37
  type _38 = argon.lang.typeclasses._38
  type _39 = argon.lang.typeclasses._39
  type _40 = argon.lang.typeclasses._40
  type _41 = argon.lang.typeclasses._41
  type _42 = argon.lang.typeclasses._42
  type _43 = argon.lang.typeclasses._43
  type _44 = argon.lang.typeclasses._44
  type _45 = argon.lang.typeclasses._45
  type _46 = argon.lang.typeclasses._46
  type _47 = argon.lang.typeclasses._47
  type _48 = argon.lang.typeclasses._48
  type _49 = argon.lang.typeclasses._49
  type _50 = argon.lang.typeclasses._50
  type _51 = argon.lang.typeclasses._51
  type _52 = argon.lang.typeclasses._52
  type _53 = argon.lang.typeclasses._53
  type _54 = argon.lang.typeclasses._54
  type _55 = argon.lang.typeclasses._55
  type _56 = argon.lang.typeclasses._56
  type _57 = argon.lang.typeclasses._57
  type _58 = argon.lang.typeclasses._58
  type _59 = argon.lang.typeclasses._59
  type _60 = argon.lang.typeclasses._60
  type _61 = argon.lang.typeclasses._61
  type _62 = argon.lang.typeclasses._62
  type _63 = argon.lang.typeclasses._63
  type _64 = argon.lang.typeclasses._64
  type _65 = argon.lang.typeclasses._65
  type _66 = argon.lang.typeclasses._66
  type _67 = argon.lang.typeclasses._67
  type _68 = argon.lang.typeclasses._68
  type _69 = argon.lang.typeclasses._69
  type _70 = argon.lang.typeclasses._70
  type _71 = argon.lang.typeclasses._71
  type _72 = argon.lang.typeclasses._72
  type _73 = argon.lang.typeclasses._73
  type _74 = argon.lang.typeclasses._74
  type _75 = argon.lang.typeclasses._75
  type _76 = argon.lang.typeclasses._76
  type _77 = argon.lang.typeclasses._77
  type _78 = argon.lang.typeclasses._78
  type _79 = argon.lang.typeclasses._79
  type _80 = argon.lang.typeclasses._80
  type _81 = argon.lang.typeclasses._81
  type _82 = argon.lang.typeclasses._82
  type _83 = argon.lang.typeclasses._83
  type _84 = argon.lang.typeclasses._84
  type _85 = argon.lang.typeclasses._85
  type _86 = argon.lang.typeclasses._86
  type _87 = argon.lang.typeclasses._87
  type _88 = argon.lang.typeclasses._88
  type _89 = argon.lang.typeclasses._89
  type _90 = argon.lang.typeclasses._90
  type _91 = argon.lang.typeclasses._91
  type _92 = argon.lang.typeclasses._92
  type _93 = argon.lang.typeclasses._93
  type _94 = argon.lang.typeclasses._94
  type _95 = argon.lang.typeclasses._95
  type _96 = argon.lang.typeclasses._96
  type _97 = argon.lang.typeclasses._97
  type _98 = argon.lang.typeclasses._98
  type _99 = argon.lang.typeclasses._99
  type _100 = argon.lang.typeclasses._100
  type _101 = argon.lang.typeclasses._101
  type _102 = argon.lang.typeclasses._102
  type _103 = argon.lang.typeclasses._103
  type _104 = argon.lang.typeclasses._104
  type _105 = argon.lang.typeclasses._105
  type _106 = argon.lang.typeclasses._106
  type _107 = argon.lang.typeclasses._107
  type _108 = argon.lang.typeclasses._108
  type _109 = argon.lang.typeclasses._109
  type _110 = argon.lang.typeclasses._110
  type _111 = argon.lang.typeclasses._111
  type _112 = argon.lang.typeclasses._112
  type _113 = argon.lang.typeclasses._113
  type _114 = argon.lang.typeclasses._114
  type _115 = argon.lang.typeclasses._115
  type _116 = argon.lang.typeclasses._116
  type _117 = argon.lang.typeclasses._117
  type _118 = argon.lang.typeclasses._118
  type _119 = argon.lang.typeclasses._119
  type _120 = argon.lang.typeclasses._120
  type _121 = argon.lang.typeclasses._121
  type _122 = argon.lang.typeclasses._122
  type _123 = argon.lang.typeclasses._123
  type _124 = argon.lang.typeclasses._124
  type _125 = argon.lang.typeclasses._125
  type _126 = argon.lang.typeclasses._126
  type _127 = argon.lang.typeclasses._127
  type _128 = argon.lang.typeclasses._128
}

/** Implicit typeclass evidence, (optionally) implicit conversions **/
trait ArgonExp
  extends ExternalAliases
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
{
  val Array = argon.lang.Array
}

object compiler extends ArgonExp {
  val INT = argon.lang.typeclasses.INT
  val BOOL = argon.lang.typeclasses.BOOL

  val MArray = argon.lang.Array
  val MHashMap = argon.lang.HashMap
  val MBoolean = argon.lang.Boolean

  val FixPt = argon.lang.FixPt
  val FltPt = argon.lang.FltPt

  val MString = argon.lang.String
  val MTuple2 = argon.lang.Tuple2

  val Struct = argon.lang.Struct

  val MUnit = argon.lang.Unit

  val Var = argon.lang.Var

  val Func = argon.lang.Func
}