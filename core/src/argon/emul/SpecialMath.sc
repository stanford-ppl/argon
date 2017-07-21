import argon.emul._

val a_usgn = FixedPoint(0.125, FixFormat(false,4,4))
val b_usgn = FixedPoint(5.625, FixFormat(false,4,4))

val c_usgn = FixedPoint(45, FixFormat(false,4,4))

5.625 / 0.125
b_usgn / a_usgn
b_usgn /& a_usgn

(b_usgn / a_usgn).value

(b_usgn /& a_usgn).value

((b_usgn.value << 8) / (a_usgn.value)) >> 4


