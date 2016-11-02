package argon.ops

/** Hack for working with customized bit widths, since Scala doesn't support integers as template parameters **/
trait CustomBitWidths {
  def sign[T:Sign] = implicitly[Sign[T]]
  def bits[T:Bits] = implicitly[Bits[T]]

  trait Sign[T] { def isSigned: Boolean }
  trait Bits[T] { def nbits: Int }

  abstract class Signed; implicit object SignedSign extends Sign[Signed] { def isSigned = true }
  abstract class Unsign; implicit object UnsignSign extends Sign[Unsign] { def isSigned = false }
  abstract class B0; implicit object Bits0 extends Bits[B0] { def nbits = 0 }
  abstract class B1; implicit object Bits1 extends Bits[B1] { def nbits = 1 }
  abstract class B2; implicit object Bits2 extends Bits[B2] { def nbits = 2 }
  abstract class B3; implicit object Bits3 extends Bits[B3] { def nbits = 3 }
  abstract class B4; implicit object Bits4 extends Bits[B4] { def nbits = 4 }
  abstract class B5; implicit object Bits5 extends Bits[B5] { def nbits = 5 }
  abstract class B6; implicit object Bits6 extends Bits[B6] { def nbits = 6 }
  abstract class B7; implicit object Bits7 extends Bits[B7] { def nbits = 7 }
  abstract class B8; implicit object Bits8 extends Bits[B8] { def nbits = 8 }
  abstract class B9; implicit object Bits9 extends Bits[B9] { def nbits = 9 }
  abstract class B10; implicit object Bits10 extends Bits[B10] { def nbits = 10 }
  abstract class B11; implicit object Bits11 extends Bits[B11] { def nbits = 11 }
  abstract class B12; implicit object Bits12 extends Bits[B12] { def nbits = 12 }
  abstract class B13; implicit object Bits13 extends Bits[B13] { def nbits = 13 }
  abstract class B14; implicit object Bits14 extends Bits[B14] { def nbits = 14 }
  abstract class B15; implicit object Bits15 extends Bits[B15] { def nbits = 15 }
  abstract class B16; implicit object Bits16 extends Bits[B16] { def nbits = 16 }
  abstract class B17; implicit object Bits17 extends Bits[B17] { def nbits = 17 }
  abstract class B18; implicit object Bits18 extends Bits[B18] { def nbits = 18 }
  abstract class B19; implicit object Bits19 extends Bits[B19] { def nbits = 19 }
  abstract class B20; implicit object Bits20 extends Bits[B20] { def nbits = 20 }
  abstract class B21; implicit object Bits21 extends Bits[B21] { def nbits = 21 }
  abstract class B22; implicit object Bits22 extends Bits[B22] { def nbits = 22 }
  abstract class B23; implicit object Bits23 extends Bits[B23] { def nbits = 23 }
  abstract class B24; implicit object Bits24 extends Bits[B24] { def nbits = 24 }
  abstract class B25; implicit object Bits25 extends Bits[B25] { def nbits = 25 }
  abstract class B26; implicit object Bits26 extends Bits[B26] { def nbits = 26 }
  abstract class B27; implicit object Bits27 extends Bits[B27] { def nbits = 27 }
  abstract class B28; implicit object Bits28 extends Bits[B28] { def nbits = 28 }
  abstract class B29; implicit object Bits29 extends Bits[B29] { def nbits = 29 }
  abstract class B30; implicit object Bits30 extends Bits[B30] { def nbits = 30 }
  abstract class B31; implicit object Bits31 extends Bits[B31] { def nbits = 31 }
  abstract class B32; implicit object Bits32 extends Bits[B32] { def nbits = 32 }
  abstract class B33; implicit object Bits33 extends Bits[B33] { def nbits = 33 }
  abstract class B34; implicit object Bits34 extends Bits[B34] { def nbits = 34 }
  abstract class B35; implicit object Bits35 extends Bits[B35] { def nbits = 35 }
  abstract class B36; implicit object Bits36 extends Bits[B36] { def nbits = 36 }
  abstract class B37; implicit object Bits37 extends Bits[B37] { def nbits = 37 }
  abstract class B38; implicit object Bits38 extends Bits[B38] { def nbits = 38 }
  abstract class B39; implicit object Bits39 extends Bits[B39] { def nbits = 39 }
  abstract class B40; implicit object Bits40 extends Bits[B40] { def nbits = 40 }
  abstract class B41; implicit object Bits41 extends Bits[B41] { def nbits = 41 }
  abstract class B42; implicit object Bits42 extends Bits[B42] { def nbits = 42 }
  abstract class B43; implicit object Bits43 extends Bits[B43] { def nbits = 43 }
  abstract class B44; implicit object Bits44 extends Bits[B44] { def nbits = 44 }
  abstract class B45; implicit object Bits45 extends Bits[B45] { def nbits = 45 }
  abstract class B46; implicit object Bits46 extends Bits[B46] { def nbits = 46 }
  abstract class B47; implicit object Bits47 extends Bits[B47] { def nbits = 47 }
  abstract class B48; implicit object Bits48 extends Bits[B48] { def nbits = 48 }
  abstract class B49; implicit object Bits49 extends Bits[B49] { def nbits = 49 }
  abstract class B50; implicit object Bits50 extends Bits[B50] { def nbits = 50 }
  abstract class B51; implicit object Bits51 extends Bits[B51] { def nbits = 51 }
  abstract class B52; implicit object Bits52 extends Bits[B52] { def nbits = 52 }
  abstract class B53; implicit object Bits53 extends Bits[B53] { def nbits = 53 }
  abstract class B54; implicit object Bits54 extends Bits[B54] { def nbits = 54 }
  abstract class B55; implicit object Bits55 extends Bits[B55] { def nbits = 55 }
  abstract class B56; implicit object Bits56 extends Bits[B56] { def nbits = 56 }
  abstract class B57; implicit object Bits57 extends Bits[B57] { def nbits = 57 }
  abstract class B58; implicit object Bits58 extends Bits[B58] { def nbits = 58 }
  abstract class B59; implicit object Bits59 extends Bits[B59] { def nbits = 59 }
  abstract class B60; implicit object Bits60 extends Bits[B60] { def nbits = 60 }
  abstract class B61; implicit object Bits61 extends Bits[B61] { def nbits = 61 }
  abstract class B62; implicit object Bits62 extends Bits[B62] { def nbits = 62 }
  abstract class B63; implicit object Bits63 extends Bits[B63] { def nbits = 63 }
  abstract class B64; implicit object Bits64 extends Bits[B64] { def nbits = 64 }
}

