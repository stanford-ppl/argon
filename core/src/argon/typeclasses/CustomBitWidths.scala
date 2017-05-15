package argon.typeclasses

import argon._
import forge._

sealed trait BOOL[T] {
  val v: Boolean
  override def toString = if (v) "TRUE" else "FALSE"
}
sealed trait INT[T] {
  val v: Int
  override def equals(x: Any): Boolean = x match {
    case that: INT[_] => this.v == that.v
    case _ => false
  }
  override def toString = s"_$v"
}

trait TRUE
trait FALSE

trait _0
trait _1
trait _2
trait _3
trait _4
trait _5
trait _6
trait _7
trait _8
trait _9
trait _10
trait _11
trait _12
trait _13
trait _14
trait _15
trait _16
trait _17
trait _18
trait _19
trait _20
trait _21
trait _22
trait _23
trait _24
trait _25
trait _26
trait _27
trait _28
trait _29
trait _30
trait _31
trait _32
trait _33
trait _34
trait _35
trait _36
trait _37
trait _38
trait _39
trait _40
trait _41
trait _42
trait _43
trait _44
trait _45
trait _46
trait _47
trait _48
trait _49
trait _50
trait _51
trait _52
trait _53
trait _54
trait _55
trait _56
trait _57
trait _58
trait _59
trait _60
trait _61
trait _62
trait _63
trait _64
trait _65
trait _66
trait _67
trait _68
trait _69
trait _70
trait _71
trait _72
trait _73
trait _74
trait _75
trait _76
trait _77
trait _78
trait _79
trait _80
trait _81
trait _82
trait _83
trait _84
trait _85
trait _86
trait _87
trait _88
trait _89
trait _90
trait _91
trait _92
trait _93
trait _94
trait _95
trait _96
trait _97
trait _98
trait _99
trait _100
trait _101
trait _102
trait _103
trait _104
trait _105
trait _106
trait _107
trait _108
trait _109
trait _110
trait _111
trait _112
trait _113
trait _114
trait _115
trait _116
trait _117
trait _118
trait _119
trait _120
trait _121
trait _122
trait _123
trait _124
trait _125
trait _126
trait _127
trait _128

/** Hack for working with customized bit widths, since Scala doesn't support integers as template parameters **/
object BOOL {
  def apply[T:BOOL]: BOOL[T] = implicitly[BOOL[T]]

  implicit object BOOL_TRUE extends BOOL[TRUE] { val v = true }
  implicit object BOOL_FALSE extends BOOL[FALSE] { val v = false }
}

object INT {
  def apply[T:INT]: INT[T] = implicitly[INT[T]]
  def from[T](x: Int): INT[T] = new INT[T] { val v = x }

  implicit object INT0  extends INT[_0]  { val v = 0  }
  implicit object INT1  extends INT[_1]  { val v = 1  }
  implicit object INT2  extends INT[_2]  { val v = 2  }
  implicit object INT3  extends INT[_3]  { val v = 3  }
  implicit object INT4  extends INT[_4]  { val v = 4  }
  implicit object INT5  extends INT[_5]  { val v = 5  }
  implicit object INT6  extends INT[_6]  { val v = 6  }
  implicit object INT7  extends INT[_7]  { val v = 7  }
  implicit object INT8  extends INT[_8]  { val v = 8  }
  implicit object INT9  extends INT[_9]  { val v = 9  }
  implicit object INT10 extends INT[_10] { val v = 10 }
  implicit object INT11 extends INT[_11] { val v = 11 }
  implicit object INT12 extends INT[_12] { val v = 12 }
  implicit object INT13 extends INT[_13] { val v = 13 }
  implicit object INT14 extends INT[_14] { val v = 14 }
  implicit object INT15 extends INT[_15] { val v = 15 }
  implicit object INT16 extends INT[_16] { val v = 16 }
  implicit object INT17 extends INT[_17] { val v = 17 }
  implicit object INT18 extends INT[_18] { val v = 18 }
  implicit object INT19 extends INT[_19] { val v = 19 }
  implicit object INT20 extends INT[_20] { val v = 20 }
  implicit object INT21 extends INT[_21] { val v = 21 }
  implicit object INT22 extends INT[_22] { val v = 22 }
  implicit object INT23 extends INT[_23] { val v = 23 }
  implicit object INT24 extends INT[_24] { val v = 24 }
  implicit object INT25 extends INT[_25] { val v = 25 }
  implicit object INT26 extends INT[_26] { val v = 26 }
  implicit object INT27 extends INT[_27] { val v = 27 }
  implicit object INT28 extends INT[_28] { val v = 28 }
  implicit object INT29 extends INT[_29] { val v = 29 }
  implicit object INT30 extends INT[_30] { val v = 30 }
  implicit object INT31 extends INT[_31] { val v = 31 }
  implicit object INT32 extends INT[_32] { val v = 32 }
  implicit object INT33 extends INT[_33] { val v = 33 }
  implicit object INT34 extends INT[_34] { val v = 34 }
  implicit object INT35 extends INT[_35] { val v = 35 }
  implicit object INT36 extends INT[_36] { val v = 36 }
  implicit object INT37 extends INT[_37] { val v = 37 }
  implicit object INT38 extends INT[_38] { val v = 38 }
  implicit object INT39 extends INT[_39] { val v = 39 }
  implicit object INT40 extends INT[_40] { val v = 40 }
  implicit object INT41 extends INT[_41] { val v = 41 }
  implicit object INT42 extends INT[_42] { val v = 42 }
  implicit object INT43 extends INT[_43] { val v = 43 }
  implicit object INT44 extends INT[_44] { val v = 44 }
  implicit object INT45 extends INT[_45] { val v = 45 }
  implicit object INT46 extends INT[_46] { val v = 46 }
  implicit object INT47 extends INT[_47] { val v = 47 }
  implicit object INT48 extends INT[_48] { val v = 48 }
  implicit object INT49 extends INT[_49] { val v = 49 }
  implicit object INT50 extends INT[_50] { val v = 50 }
  implicit object INT51 extends INT[_51] { val v = 51 }
  implicit object INT52 extends INT[_52] { val v = 52 }
  implicit object INT53 extends INT[_53] { val v = 53 }
  implicit object INT54 extends INT[_54] { val v = 54 }
  implicit object INT55 extends INT[_55] { val v = 55 }
  implicit object INT56 extends INT[_56] { val v = 56 }
  implicit object INT57 extends INT[_57] { val v = 57 }
  implicit object INT58 extends INT[_58] { val v = 58 }
  implicit object INT59 extends INT[_59] { val v = 59 }
  implicit object INT60 extends INT[_60] { val v = 60 }
  implicit object INT61 extends INT[_61] { val v = 61 }
  implicit object INT62 extends INT[_62] { val v = 62 }
  implicit object INT63 extends INT[_63] { val v = 63 }
  implicit object INT64 extends INT[_64] { val v = 64 }
  implicit object INT65 extends INT[_65] { val v = 65 }
  implicit object INT66 extends INT[_66] { val v = 66 }
  implicit object INT67 extends INT[_67] { val v = 67 }
  implicit object INT68 extends INT[_68] { val v = 68 }
  implicit object INT69 extends INT[_69] { val v = 69 }
  implicit object INT70 extends INT[_70] { val v = 70 }
  implicit object INT71 extends INT[_71] { val v = 71 }
  implicit object INT72 extends INT[_72] { val v = 72 }
  implicit object INT73 extends INT[_73] { val v = 73 }
  implicit object INT74 extends INT[_74] { val v = 74 }
  implicit object INT75 extends INT[_75] { val v = 75 }
  implicit object INT76 extends INT[_76] { val v = 76 }
  implicit object INT77 extends INT[_77] { val v = 77 }
  implicit object INT78 extends INT[_78] { val v = 78 }
  implicit object INT79 extends INT[_79] { val v = 79 }
  implicit object INT80 extends INT[_80] { val v = 80 }
  implicit object INT81 extends INT[_81] { val v = 81 }
  implicit object INT82 extends INT[_82] { val v = 82 }
  implicit object INT83 extends INT[_83] { val v = 83 }
  implicit object INT84 extends INT[_84] { val v = 84 }
  implicit object INT85 extends INT[_85] { val v = 85 }
  implicit object INT86 extends INT[_86] { val v = 86 }
  implicit object INT87 extends INT[_87] { val v = 87 }
  implicit object INT88 extends INT[_88] { val v = 88 }
  implicit object INT89 extends INT[_89] { val v = 89 }
  implicit object INT90 extends INT[_90] { val v = 90 }
  implicit object INT91 extends INT[_91] { val v = 91 }
  implicit object INT92 extends INT[_92] { val v = 92 }
  implicit object INT93 extends INT[_93] { val v = 93 }
  implicit object INT94 extends INT[_94] { val v = 94 }
  implicit object INT95 extends INT[_95] { val v = 95 }
  implicit object INT96 extends INT[_96] { val v = 96 }
  implicit object INT97 extends INT[_97] { val v = 97 }
  implicit object INT98 extends INT[_98] { val v = 98 }
  implicit object INT99 extends INT[_99] { val v = 99 }

  implicit object INT100 extends INT[_100] { val v = 100 }
  implicit object INT101 extends INT[_101] { val v = 101 }
  implicit object INT102 extends INT[_102] { val v = 102 }
  implicit object INT103 extends INT[_103] { val v = 103 }
  implicit object INT104 extends INT[_104] { val v = 104 }
  implicit object INT105 extends INT[_105] { val v = 105 }
  implicit object INT106 extends INT[_106] { val v = 106 }
  implicit object INT107 extends INT[_107] { val v = 107 }
  implicit object INT108 extends INT[_108] { val v = 108 }
  implicit object INT109 extends INT[_109] { val v = 109 }
  implicit object INT110 extends INT[_110] { val v = 110 }
  implicit object INT111 extends INT[_111] { val v = 111 }
  implicit object INT112 extends INT[_112] { val v = 112 }
  implicit object INT113 extends INT[_113] { val v = 113 }
  implicit object INT114 extends INT[_114] { val v = 114 }
  implicit object INT115 extends INT[_115] { val v = 115 }
  implicit object INT116 extends INT[_116] { val v = 116 }
  implicit object INT117 extends INT[_117] { val v = 117 }
  implicit object INT118 extends INT[_118] { val v = 118 }
  implicit object INT119 extends INT[_119] { val v = 119 }
  implicit object INT120 extends INT[_120] { val v = 120 }
  implicit object INT121 extends INT[_121] { val v = 121 }
  implicit object INT122 extends INT[_122] { val v = 122 }
  implicit object INT123 extends INT[_123] { val v = 123 }
  implicit object INT124 extends INT[_124] { val v = 124 }
  implicit object INT125 extends INT[_125] { val v = 125 }
  implicit object INT126 extends INT[_126] { val v = 126 }
  implicit object INT127 extends INT[_127] { val v = 127 }
  implicit object INT128 extends INT[_128] { val v = 128 }
}
