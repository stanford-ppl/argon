package argon.ops
import argon.core.Reporting

/** Hack for working with customized bit widths, since Scala doesn't support integers as template parameters **/
trait CustomBitWidths extends Reporting {
  def BOOL[T:BOOL] = implicitly[BOOL[T]]
  def INT[T:INT] = implicitly[INT[T]]

  sealed abstract class BOOL[T] { val v: Boolean }
  sealed abstract class INT[T] { val v: Int }

  trait TRUE;  implicit object BOOL_TRUE extends BOOL[TRUE] { val v = true }
  trait FALSE; implicit object BOOL_FALSE extends BOOL[FALSE] { val v = false }
  trait _0;  implicit object INT0  extends INT[_0]  { val v = 0  }
  trait _1;  implicit object INT1  extends INT[_1]  { val v = 1  }
  trait _2;  implicit object INT2  extends INT[_2]  { val v = 2  }
  trait _3;  implicit object INT3  extends INT[_3]  { val v = 3  }
  trait _4;  implicit object INT4  extends INT[_4]  { val v = 4  }
  trait _5;  implicit object INT5  extends INT[_5]  { val v = 5  }
  trait _6;  implicit object INT6  extends INT[_6]  { val v = 6  }
  trait _7;  implicit object INT7  extends INT[_7]  { val v = 7  }
  trait _8;  implicit object INT8  extends INT[_8]  { val v = 8  }
  trait _9;  implicit object INT9  extends INT[_9]  { val v = 9  }
  trait _10; implicit object INT10 extends INT[_10] { val v = 10 }
  trait _11; implicit object INT11 extends INT[_11] { val v = 11 }
  trait _12; implicit object INT12 extends INT[_12] { val v = 12 }
  trait _13; implicit object INT13 extends INT[_13] { val v = 13 }
  trait _14; implicit object INT14 extends INT[_14] { val v = 14 }
  trait _15; implicit object INT15 extends INT[_15] { val v = 15 }
  trait _16; implicit object INT16 extends INT[_16] { val v = 16 }
  trait _17; implicit object INT17 extends INT[_17] { val v = 17 }
  trait _18; implicit object INT18 extends INT[_18] { val v = 18 }
  trait _19; implicit object INT19 extends INT[_19] { val v = 19 }
  trait _20; implicit object INT20 extends INT[_20] { val v = 20 }
  trait _21; implicit object INT21 extends INT[_21] { val v = 21 }
  trait _22; implicit object INT22 extends INT[_22] { val v = 22 }
  trait _23; implicit object INT23 extends INT[_23] { val v = 23 }
  trait _24; implicit object INT24 extends INT[_24] { val v = 24 }
  trait _25; implicit object INT25 extends INT[_25] { val v = 25 }
  trait _26; implicit object INT26 extends INT[_26] { val v = 26 }
  trait _27; implicit object INT27 extends INT[_27] { val v = 27 }
  trait _28; implicit object INT28 extends INT[_28] { val v = 28 }
  trait _29; implicit object INT29 extends INT[_29] { val v = 29 }
  trait _30; implicit object INT30 extends INT[_30] { val v = 30 }
  trait _31; implicit object INT31 extends INT[_31] { val v = 31 }
  trait _32; implicit object INT32 extends INT[_32] { val v = 32 }
  trait _33; implicit object INT33 extends INT[_33] { val v = 33 }
  trait _34; implicit object INT34 extends INT[_34] { val v = 34 }
  trait _35; implicit object INT35 extends INT[_35] { val v = 35 }
  trait _36; implicit object INT36 extends INT[_36] { val v = 36 }
  trait _37; implicit object INT37 extends INT[_37] { val v = 37 }
  trait _38; implicit object INT38 extends INT[_38] { val v = 38 }
  trait _39; implicit object INT39 extends INT[_39] { val v = 39 }
  trait _40; implicit object INT40 extends INT[_40] { val v = 40 }
  trait _41; implicit object INT41 extends INT[_41] { val v = 41 }
  trait _42; implicit object INT42 extends INT[_42] { val v = 42 }
  trait _43; implicit object INT43 extends INT[_43] { val v = 43 }
  trait _44; implicit object INT44 extends INT[_44] { val v = 44 }
  trait _45; implicit object INT45 extends INT[_45] { val v = 45 }
  trait _46; implicit object INT46 extends INT[_46] { val v = 46 }
  trait _47; implicit object INT47 extends INT[_47] { val v = 47 }
  trait _48; implicit object INT48 extends INT[_48] { val v = 48 }
  trait _49; implicit object INT49 extends INT[_49] { val v = 49 }
  trait _50; implicit object INT50 extends INT[_50] { val v = 50 }
  trait _51; implicit object INT51 extends INT[_51] { val v = 51 }
  trait _52; implicit object INT52 extends INT[_52] { val v = 52 }
  trait _53; implicit object INT53 extends INT[_53] { val v = 53 }
  trait _54; implicit object INT54 extends INT[_54] { val v = 54 }
  trait _55; implicit object INT55 extends INT[_55] { val v = 55 }
  trait _56; implicit object INT56 extends INT[_56] { val v = 56 }
  trait _57; implicit object INT57 extends INT[_57] { val v = 57 }
  trait _58; implicit object INT58 extends INT[_58] { val v = 58 }
  trait _59; implicit object INT59 extends INT[_59] { val v = 59 }
  trait _60; implicit object INT60 extends INT[_60] { val v = 60 }
  trait _61; implicit object INT61 extends INT[_61] { val v = 61 }
  trait _62; implicit object INT62 extends INT[_62] { val v = 62 }
  trait _63; implicit object INT63 extends INT[_63] { val v = 63 }
  trait _64; implicit object INT64 extends INT[_64] { val v = 64 }

  override def userReadable(x: Any): String = x match {
    case x:INT[_] => s"_${x.v}"
    case x:BOOL[_] => if (x.v) "TRUE" else "FALSE"
    case _ => super.userReadable(x)
  }

}

