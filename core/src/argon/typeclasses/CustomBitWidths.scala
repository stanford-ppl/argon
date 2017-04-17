package argon.typeclasses

import argon.core.Reporting
import forge._

/** Hack for working with customized bit widths, since Scala doesn't support integers as template parameters **/
trait CustomBitWidths extends Reporting {
  def BOOL[T: BOOL]: BOOL[T]   = implicitly[BOOL[T]]
  def INT[T: INT]: INT[T]      = implicitly[INT[T]]
  def INT_T[T](x: Int): INT[T] = new INT[T] { val v = x }

  sealed abstract class BOOL[T] { val v: Boolean }
  sealed abstract class INT[T] {
    val v: Int
    override def equals(x: Any): Boolean = x match {
      case that: INT[_] => this.v == that.v
      case _            => false
    }
  }

  trait TRUE; implicit object BOOL_TRUE   extends BOOL[TRUE]  { val v = true  }
  trait FALSE; implicit object BOOL_FALSE extends BOOL[FALSE] { val v = false }

  /*@generate
  trait _JJ$JJ$0to128

  @generate
  implicit object INTJJ$JJ$0to128 extends INT[_JJ] { val v = JJ }*/

  trait _0; implicit object INT0     extends INT[_0]   { val v = 0   }
  trait _1; implicit object INT1     extends INT[_1]   { val v = 1   }
  trait _2; implicit object INT2     extends INT[_2]   { val v = 2   }
  trait _3; implicit object INT3     extends INT[_3]   { val v = 3   }
  trait _4; implicit object INT4     extends INT[_4]   { val v = 4   }
  trait _5; implicit object INT5     extends INT[_5]   { val v = 5   }
  trait _6; implicit object INT6     extends INT[_6]   { val v = 6   }
  trait _7; implicit object INT7     extends INT[_7]   { val v = 7   }
  trait _8; implicit object INT8     extends INT[_8]   { val v = 8   }
  trait _9; implicit object INT9     extends INT[_9]   { val v = 9   }
  trait _10; implicit object INT10   extends INT[_10]  { val v = 10  }
  trait _11; implicit object INT11   extends INT[_11]  { val v = 11  }
  trait _12; implicit object INT12   extends INT[_12]  { val v = 12  }
  trait _13; implicit object INT13   extends INT[_13]  { val v = 13  }
  trait _14; implicit object INT14   extends INT[_14]  { val v = 14  }
  trait _15; implicit object INT15   extends INT[_15]  { val v = 15  }
  trait _16; implicit object INT16   extends INT[_16]  { val v = 16  }
  trait _17; implicit object INT17   extends INT[_17]  { val v = 17  }
  trait _18; implicit object INT18   extends INT[_18]  { val v = 18  }
  trait _19; implicit object INT19   extends INT[_19]  { val v = 19  }
  trait _20; implicit object INT20   extends INT[_20]  { val v = 20  }
  trait _21; implicit object INT21   extends INT[_21]  { val v = 21  }
  trait _22; implicit object INT22   extends INT[_22]  { val v = 22  }
  trait _23; implicit object INT23   extends INT[_23]  { val v = 23  }
  trait _24; implicit object INT24   extends INT[_24]  { val v = 24  }
  trait _25; implicit object INT25   extends INT[_25]  { val v = 25  }
  trait _26; implicit object INT26   extends INT[_26]  { val v = 26  }
  trait _27; implicit object INT27   extends INT[_27]  { val v = 27  }
  trait _28; implicit object INT28   extends INT[_28]  { val v = 28  }
  trait _29; implicit object INT29   extends INT[_29]  { val v = 29  }
  trait _30; implicit object INT30   extends INT[_30]  { val v = 30  }
  trait _31; implicit object INT31   extends INT[_31]  { val v = 31  }
  trait _32; implicit object INT32   extends INT[_32]  { val v = 32  }
  trait _33; implicit object INT33   extends INT[_33]  { val v = 33  }
  trait _34; implicit object INT34   extends INT[_34]  { val v = 34  }
  trait _35; implicit object INT35   extends INT[_35]  { val v = 35  }
  trait _36; implicit object INT36   extends INT[_36]  { val v = 36  }
  trait _37; implicit object INT37   extends INT[_37]  { val v = 37  }
  trait _38; implicit object INT38   extends INT[_38]  { val v = 38  }
  trait _39; implicit object INT39   extends INT[_39]  { val v = 39  }
  trait _40; implicit object INT40   extends INT[_40]  { val v = 40  }
  trait _41; implicit object INT41   extends INT[_41]  { val v = 41  }
  trait _42; implicit object INT42   extends INT[_42]  { val v = 42  }
  trait _43; implicit object INT43   extends INT[_43]  { val v = 43  }
  trait _44; implicit object INT44   extends INT[_44]  { val v = 44  }
  trait _45; implicit object INT45   extends INT[_45]  { val v = 45  }
  trait _46; implicit object INT46   extends INT[_46]  { val v = 46  }
  trait _47; implicit object INT47   extends INT[_47]  { val v = 47  }
  trait _48; implicit object INT48   extends INT[_48]  { val v = 48  }
  trait _49; implicit object INT49   extends INT[_49]  { val v = 49  }
  trait _50; implicit object INT50   extends INT[_50]  { val v = 50  }
  trait _51; implicit object INT51   extends INT[_51]  { val v = 51  }
  trait _52; implicit object INT52   extends INT[_52]  { val v = 52  }
  trait _53; implicit object INT53   extends INT[_53]  { val v = 53  }
  trait _54; implicit object INT54   extends INT[_54]  { val v = 54  }
  trait _55; implicit object INT55   extends INT[_55]  { val v = 55  }
  trait _56; implicit object INT56   extends INT[_56]  { val v = 56  }
  trait _57; implicit object INT57   extends INT[_57]  { val v = 57  }
  trait _58; implicit object INT58   extends INT[_58]  { val v = 58  }
  trait _59; implicit object INT59   extends INT[_59]  { val v = 59  }
  trait _60; implicit object INT60   extends INT[_60]  { val v = 60  }
  trait _61; implicit object INT61   extends INT[_61]  { val v = 61  }
  trait _62; implicit object INT62   extends INT[_62]  { val v = 62  }
  trait _63; implicit object INT63   extends INT[_63]  { val v = 63  }
  trait _64; implicit object INT64   extends INT[_64]  { val v = 64  }
  trait _65; implicit object INT65   extends INT[_65]  { val v = 65  }
  trait _66; implicit object INT66   extends INT[_66]  { val v = 66  }
  trait _67; implicit object INT67   extends INT[_67]  { val v = 67  }
  trait _68; implicit object INT68   extends INT[_68]  { val v = 68  }
  trait _69; implicit object INT69   extends INT[_69]  { val v = 69  }
  trait _70; implicit object INT70   extends INT[_70]  { val v = 70  }
  trait _71; implicit object INT71   extends INT[_71]  { val v = 71  }
  trait _72; implicit object INT72   extends INT[_72]  { val v = 72  }
  trait _73; implicit object INT73   extends INT[_73]  { val v = 73  }
  trait _74; implicit object INT74   extends INT[_74]  { val v = 74  }
  trait _75; implicit object INT75   extends INT[_75]  { val v = 75  }
  trait _76; implicit object INT76   extends INT[_76]  { val v = 76  }
  trait _77; implicit object INT77   extends INT[_77]  { val v = 77  }
  trait _78; implicit object INT78   extends INT[_78]  { val v = 78  }
  trait _79; implicit object INT79   extends INT[_79]  { val v = 79  }
  trait _80; implicit object INT80   extends INT[_80]  { val v = 80  }
  trait _81; implicit object INT81   extends INT[_81]  { val v = 81  }
  trait _82; implicit object INT82   extends INT[_82]  { val v = 82  }
  trait _83; implicit object INT83   extends INT[_83]  { val v = 83  }
  trait _84; implicit object INT84   extends INT[_84]  { val v = 84  }
  trait _85; implicit object INT85   extends INT[_85]  { val v = 85  }
  trait _86; implicit object INT86   extends INT[_86]  { val v = 86  }
  trait _87; implicit object INT87   extends INT[_87]  { val v = 87  }
  trait _88; implicit object INT88   extends INT[_88]  { val v = 88  }
  trait _89; implicit object INT89   extends INT[_89]  { val v = 89  }
  trait _90; implicit object INT90   extends INT[_90]  { val v = 90  }
  trait _91; implicit object INT91   extends INT[_91]  { val v = 91  }
  trait _92; implicit object INT92   extends INT[_92]  { val v = 92  }
  trait _93; implicit object INT93   extends INT[_93]  { val v = 93  }
  trait _94; implicit object INT94   extends INT[_94]  { val v = 94  }
  trait _95; implicit object INT95   extends INT[_95]  { val v = 95  }
  trait _96; implicit object INT96   extends INT[_96]  { val v = 96  }
  trait _97; implicit object INT97   extends INT[_97]  { val v = 97  }
  trait _98; implicit object INT98   extends INT[_98]  { val v = 98  }
  trait _99; implicit object INT99   extends INT[_99]  { val v = 99  }
  trait _100; implicit object INT100 extends INT[_100] { val v = 100 }
  trait _101; implicit object INT101 extends INT[_101] { val v = 101 }
  trait _102; implicit object INT102 extends INT[_102] { val v = 102 }
  trait _103; implicit object INT103 extends INT[_103] { val v = 103 }
  trait _104; implicit object INT104 extends INT[_104] { val v = 104 }
  trait _105; implicit object INT105 extends INT[_105] { val v = 105 }
  trait _106; implicit object INT106 extends INT[_106] { val v = 106 }
  trait _107; implicit object INT107 extends INT[_107] { val v = 107 }
  trait _108; implicit object INT108 extends INT[_108] { val v = 108 }
  trait _109; implicit object INT109 extends INT[_109] { val v = 109 }
  trait _110; implicit object INT110 extends INT[_110] { val v = 110 }
  trait _111; implicit object INT111 extends INT[_111] { val v = 111 }
  trait _112; implicit object INT112 extends INT[_112] { val v = 112 }
  trait _113; implicit object INT113 extends INT[_113] { val v = 113 }
  trait _114; implicit object INT114 extends INT[_114] { val v = 114 }
  trait _115; implicit object INT115 extends INT[_115] { val v = 115 }
  trait _116; implicit object INT116 extends INT[_116] { val v = 116 }
  trait _117; implicit object INT117 extends INT[_117] { val v = 117 }
  trait _118; implicit object INT118 extends INT[_118] { val v = 118 }
  trait _119; implicit object INT119 extends INT[_119] { val v = 119 }
  trait _120; implicit object INT120 extends INT[_120] { val v = 120 }
  trait _121; implicit object INT121 extends INT[_121] { val v = 121 }
  trait _122; implicit object INT122 extends INT[_122] { val v = 122 }
  trait _123; implicit object INT123 extends INT[_123] { val v = 123 }
  trait _124; implicit object INT124 extends INT[_124] { val v = 124 }
  trait _125; implicit object INT125 extends INT[_125] { val v = 125 }
  trait _126; implicit object INT126 extends INT[_126] { val v = 126 }
  trait _127; implicit object INT127 extends INT[_127] { val v = 127 }
  trait _128; implicit object INT128 extends INT[_128] { val v = 128 }

  override def userReadable(x: Any): String = x match {
    case x: INT[_]  => s"_${x.v}"
    case x: BOOL[_] => if (x.v) "TRUE" else "FALSE"
    case _          => super.userReadable(x)
  }

}
