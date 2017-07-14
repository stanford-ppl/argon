class Bool(val value: Boolean, val valid: Boolean) {
  def &&(that: Bool)  = Bool(this.value && that.value, this.valid && that.valid)
  def ||(that: Bool)  = Bool(this.value || that.value, this.valid && that.valid)
  def ^(that: Bool)   = Bool(this.value ^ that.value, this.valid && that.valid)
  def !==(that: Bool) = Bool(this.value != that.value, this.valid && that.valid)
  def ===(that: Bool) = Bool(this.value == that.value, this.valid && that.valid)

  override def toString = if (valid) { value.toString } else "X"
}

object Bool {
  def apply(value: Boolean): Bool = new Bool(value, valid=true)
  def apply(value: Boolean, valid: Boolean): Bool = new Bool(value, valid)
}




case class FixFormat(sign: Boolean, ibits: Int, fbits: Int) {
  lazy val bits = ibits + fbits
  lazy val MAX_INTEGRAL_VALUE: BigInt = (if (sign) (BigInt(1) << (ibits-1)) - 1      else (BigInt(1) << ibits) - 1) << fbits
  lazy val MIN_INTEGRAL_VALUE: BigInt = if (sign) -(BigInt(1) << (ibits-1)) << fbits else BigInt(0)

  lazy val MAX_VALUE: BigInt = if (sign) (BigInt(1) << (ibits+fbits-1)) - 1 else (BigInt(1) << (ibits+fbits)) - 1
  lazy val MIN_VALUE: BigInt = if (sign) -(BigInt(1) << (ibits+fbits-1))    else BigInt(0)

  lazy val MAX_INTEGRAL_VALUE_FP: FixedPoint = FixedPoint.clamped(MAX_INTEGRAL_VALUE, valid=true, this)
  lazy val MIN_INTEGRAL_VALUE_FP: FixedPoint = FixedPoint.clamped(MIN_INTEGRAL_VALUE, valid=true, this)
  lazy val MAX_VALUE_FP: FixedPoint = FixedPoint.clamped(MAX_VALUE, valid=true, this)
  lazy val MIN_VALUE_FP: FixedPoint = FixedPoint.clamped(MIN_VALUE, valid=true, this)
}

class FixedPoint(val value: BigInt, val valid: Boolean, val fmt: FixFormat) {
  // All operations assume that both the left and right hand side have the same fixed point format
  def +(that: FixedPoint) = FixedPoint.clamped(this.value + that.value, this.valid && that.valid, fmt)
  def -(that: FixedPoint) = FixedPoint.clamped(this.value - that.value, this.valid && that.valid, fmt)
  def *(that: FixedPoint) = FixedPoint.clamped((this.value * that.value) >> fmt.fbits, this.valid && that.valid, fmt)
  def /(that: FixedPoint) = valueOrX{ FixedPoint.clamped((this.value << fmt.fbits) / that.value, this.valid && that.valid, fmt) }
  def %(that: FixedPoint) = valueOrX{
    val result = this.value % that.value
    val posResult = if (result < 0) result + that.value else result
    FixedPoint.clamped(posResult, this.valid && that.valid, fmt)
  }
  def &(that: FixedPoint) = FixedPoint.clamped(this.value & that.value, this.valid && that.valid, fmt)
  def ^(that: FixedPoint) = FixedPoint.clamped(this.value ^ that.value, this.valid && that.valid, fmt)
  def |(that: FixedPoint) = FixedPoint.clamped(this.value | that.value, this.valid && that.valid, fmt)

  def <(that: FixedPoint)   = Bool(this.value < that.value, this.valid && that.valid)
  def <=(that: FixedPoint)  = Bool(this.value <= that.value, this.valid && that.valid)
  def >(that: FixedPoint)   = Bool(this.value > that.value, this.valid && that.valid)
  def >=(that: FixedPoint)  = Bool(this.value >= that.value, this.valid && that.valid)
  def !==(that: FixedPoint) = Bool(this.value != that.value, this.valid && that.valid)
  def ===(that: FixedPoint) = Bool(this.value == that.value, this.valid && that.valid)

  def bits: BitArray = BitArray.tabulate(fmt.bits){i => Bool(value.testBit(i)) }

  /*def <+>(that: FixedPoint) = FixedPoint.saturating(this.value + that.value, this.valid && that.valid, fmt)
  def <->(that: FixedPoint) = FixedPoint.saturating(this.value - that.value, this.valid && that.valid, fmt)
  def <*>(that: FixedPoint) = FixedPoint.saturating(this.value * that.value, this.valid && that.valid, fmt)
  def </>(that: FixedPoint) = FixedPoint.saturating(this.value / that.value, this.valid && that.valid, fmt)
  def *&(that: FixedPoint)  = FixedPoint.unbiased(this.value * that.value, this.valid && that.valid, fmt)
  def /&(that: FixedPoint)  = valueOrX { FixedPoint.unbiased(this.value / that.value, this.valid && that.valid, fmt) }
  def <*&>(that: FixedPoint) = FixedPoint.unbiasedSat(this.value * that.value, this.valid && that.valid, fmt)
  def </&>(that: FixedPoint) = valueOrX { FixedPoint.unbiasedSat(this.value / that.value, this.valid && that.valid, fmt) }*/

  def valueOrX(x: => FixedPoint): FixedPoint = {
    try { x } catch { case _: Throwable => FixedPoint.invalid(fmt) }
  }
  override def toString: String = if (valid) {
    if (fmt.fbits > 0) {
      (BigDecimal(this.value) / BigDecimal(BigInt(1) << fmt.fbits)).bigDecimal.toPlainString
    }
    else {
      value.toString
    }
  } else "X"
}

object FixedPoint {
  def apply(x: Byte, fmt: FixFormat): FixedPoint = FixedPoint.clamped(BigInt(x) << fmt.fbits, valid=true, fmt)
  def apply(x: Short, fmt: FixFormat): FixedPoint = FixedPoint.clamped(BigInt(x) << fmt.fbits, valid=true, fmt)
  def apply(x: Int, fmt: FixFormat): FixedPoint = FixedPoint.clamped(BigInt(x) << fmt.fbits, valid=true, fmt)
  def apply(x: Long, fmt: FixFormat): FixedPoint = FixedPoint.clamped(BigInt(x) << fmt.fbits, valid=true, fmt)
  def apply(x: BigInt, fmt: FixFormat): FixedPoint = FixedPoint.clamped(x << fmt.fbits, valid=true, fmt)

  def apply(x: Float, fmt: FixFormat): FixedPoint = FixedPoint.clamped(BigDecimal(x.toDouble) * Math.pow(2,fmt.fbits), valid=true, fmt)
  def apply(x: Double, fmt: FixFormat): FixedPoint = FixedPoint.clamped(BigDecimal(x) * Math.pow(2,fmt.fbits), valid=true, fmt)
  def apply(x: BigDecimal, fmt: FixFormat): FixedPoint = FixedPoint.clamped(x, valid=true, fmt)

  def invalid(fmt: FixFormat) = new FixedPoint(-1, valid=false, fmt)
  def clamped(value: BigDecimal, valid: Boolean, fmt: FixFormat): FixedPoint = clamped(value.toBigInt, valid, fmt)
  def clamped(value: BigInt, valid: Boolean, fmt: FixFormat): FixedPoint = {
    val clampedValue = if (fmt.sign && value.testBit(fmt.bits-1)) {
      value | fmt.MIN_VALUE
    }
    else value & fmt.MAX_VALUE
    new FixedPoint(clampedValue, valid, fmt)
  }
}

class BitArray(val bits: Array[Bool]) {
  override def toString = "0b" + bits.reverse.map{b =>
    if (b.valid && b.value) "1"
    else if (b.valid && !b.value) "0"
    else "X"
  }.mkString("")
}
object BitArray {
  def tabulate(n: Int)(func: Int => Bool) = new BitArray(Array.tabulate(n)(func))
}
implicit class BigIntBits(x: BigInt) {
  def bits = BitArray.tabulate(32){i => Bool(x.testBit(i)) }
}

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

val neg4 = BigInt(-4)
neg4.bits
val negF = BigInt(-15)
negF.bits

val or = neg4 | negF
or.bits


val neg500 = FixedPoint(-500, Custom)
BigInt(-500).bits
neg500.bits

-500.toByte

val p32_52 = FixedPoint(-127.88, Custom)
p32_52.bits

p3 / p4

p32_52 % p4