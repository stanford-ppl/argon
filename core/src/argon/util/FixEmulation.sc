import argon.util.Bool
import argon.util.FixedPoint
import argon.util.FixFormat

val Custom = FixFormat(true,8,4)
val max = Custom.MAX_VALUE_FP
max.bits
val min = Custom.MIN_VALUE_FP
min.bits

val x = FixedPoint(-1, Custom)
x.bits

val y = FixedPoint(12, Custom)
y.bits

val nF = FixedPoint(-15, Custom)
val nE = FixedPoint(-14, Custom)
val nD = FixedPoint(-13, Custom)
val nC = FixedPoint(-12, Custom)
val nB = FixedPoint(-11, Custom)
val nA = FixedPoint(-10, Custom)
val n9 = FixedPoint(-9, Custom)
val n8 = FixedPoint(-8, Custom)
val n7 = FixedPoint(-7, Custom)
val n6 = FixedPoint(-6, Custom)
val n5 = FixedPoint(-5, Custom)
val n4 = FixedPoint(-4, Custom)
val n3 = FixedPoint(-3, Custom)
val n2 = FixedPoint(-2, Custom)
val n1 = FixedPoint(-1, Custom)
val p0 = FixedPoint(0, Custom)
val p1 = FixedPoint(1, Custom)
val p2 = FixedPoint(2, Custom)
val p3 = FixedPoint(3, Custom)
val p4 = FixedPoint(4, Custom)
val p5 = FixedPoint(5, Custom)
val p6 = FixedPoint(6, Custom)
val p7 = FixedPoint(7, Custom)
val p8 = FixedPoint(8, Custom)
val p9 = FixedPoint(9, Custom)
val pA = FixedPoint(10, Custom)
val pB = FixedPoint(11, Custom)
val pC = FixedPoint(12, Custom)
val pD = FixedPoint(13, Custom)
val pE = FixedPoint(14, Custom)
val pF = FixedPoint(15, Custom)


val neg500 = FixedPoint(-500, Custom)
neg500.bits

-500.toByte

val p32_52 = FixedPoint(-127.88, Custom)
p32_52.bits

p3 / p4

p32_52 % p4

