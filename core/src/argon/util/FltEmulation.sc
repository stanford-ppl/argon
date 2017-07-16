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

case class FltFormat(sbits: Int, ebits: Int) {
  lazy val bias: BigInt = BigInt(2).pow(ebits - 1) - 1
  lazy val MIN_E: BigInt = -bias + 1  // Represented as all 0s
  lazy val MAX_E: BigInt = bias       // Represented as all 1s
  lazy val SUB_E: BigInt = MIN_E - sbits
}

protected sealed abstract class FloatValue {
  def negative: Boolean
  def bits(fmt: FltFormat): Array[Bool]
  def unary_-(): FloatValue = this match {
    case NaN => NaN
    case i: Inf => Inf(!i.negative)
    case z: Zero => Zero(!z.negative)
    case v: Value => Value(-v.value)
  }
  def +(that: FloatValue): FloatValue = (this, that) match {
    case (_, NaN) => NaN
    case (NaN, _) => NaN
    case (a:Inf, b:Inf) => if (a.negative != b.negative) NaN else a
    case (_:Inf, _) => this
    case (_, _:Inf) => that
    case (_:Zero, _:Zero) => Zero(negative=false)
    case (_:Zero, _) => that
    case (_, _:Zero) => this
    case (a:Value, b:Value) => Value(a.value + b.value)
  }
  def -(that: FloatValue): FloatValue = this + (-that)
  def *(that: FloatValue): FloatValue = (this, that) match {
    case (_, NaN) => NaN
    case (NaN, _) => NaN
    case (a:Inf, b:Inf) => Inf(a.negative != b.negative)
    case (a:Inf, b:Zero) => NaN
    case (a:Zero, b:Inf) => NaN
    case (a:Inf, b:Value) => Inf(a.negative != b.negative)
    case (a:Value, b:Inf) => Inf(a.negative != b.negative)
    case (_:Inf, _) => this
    case (_, _:Inf) => that
    case (a:Zero, b:Zero) => Zero(a.negative != b.negative)
    case (_:Zero, _) => that
    case (_, _:Zero) => this
    case (a:Value,b:Value) => Value(a.value * b.value)
  }
  def /(that: FloatValue): FloatValue = (this, that) match {
    case (_, NaN) => NaN
    case (NaN, _) => NaN
    case (_:Inf,_:Inf) => NaN
    case (_:Zero,_:Zero) => NaN
    case (a:Inf, b:Value) => Inf(a.negative != b.negative)
    case (a, b:Zero) => Inf(a.negative != b.negative)
    case (a, b:Inf) => Zero(a.negative != b.negative)
    case (a:Zero, b:Value) => Zero(a.negative != b.negative)
    case (a:Value,b:Value) => Value(a.value / b.value)
  }
  def %(that: FloatValue): FloatValue = (this, that) match {
    case (_, NaN) => NaN
    case (NaN, _) => NaN
    case (_:Inf, _) => NaN
    case (a, _:Inf) => a
    case (_, _:Zero) => NaN
    case (_:Zero, _) => this
    case (a:Value, b:Value) => Value(a.value % b.value)
  }
  def <(that: FloatValue): Boolean = (this, that) match {
    case (_, NaN) => false
    case (NaN, _) => false
    case (a:Inf, b:Inf) => a.negative && !b.negative
    case (a, b:Inf) => !b.negative
    case (a:Inf, b) => b.negative
    case (a:Zero, b:Zero) => false
    case (a, b:Zero) => a.negative
    case (a:Zero, b) => !b.negative
    case (a:Value,b:Value) => a.value < b.value
  }
  def <=(that: FloatValue): Boolean = this < that || this === that
  def >(that: FloatValue): Boolean = that < this
  def >=(that: FloatValue): Boolean = that <= this
  def ===(that: FloatValue): Boolean = (this, that) match {
    case (_, NaN) => false
    case (NaN, _) => false
    case (a:Inf, b:Inf) => a.negative == b.negative
    case (_, _:Inf) => false
    case (_:Inf, _) => false
    case (a:Zero, b:Zero) => true
    case (_:Zero, _) => false
    case (_, _:Zero) => false
    case (a:Value,b:Value) => a.value == b.value
  }
  def !==(that: FloatValue): Boolean = !(this === that)
}
protected case object NaN extends FloatValue {
  val negative = false
  override def toString: String = "NaN"

  def bits(fmt: FltFormat): Array[Bool] = {
    Array.tabulate(fmt.sbits){i => Bool(i == fmt.sbits-1) } ++
    Array.tabulate(fmt.ebits){i => Bool(true) } ++
    Array(Bool(false))
  }
}
protected case class Inf(negative: Boolean) extends FloatValue {
  override def toString: String = if (negative) "-Inf" else "Inf"

  def bits(fmt: FltFormat): Array[Bool] = {
    Array.tabulate(fmt.sbits){i => Bool(false) } ++
    Array.tabulate(fmt.ebits){i => Bool(true) } ++
    Array(Bool(negative))
  }
}
protected case class Zero(negative: Boolean) extends FloatValue {
  override def toString: String = if (negative) "-0.0" else "0.0"

  def bits(fmt: FltFormat): Array[Bool] = {
    Array.tabulate(fmt.sbits){i => Bool(false) } ++
    Array.tabulate(fmt.ebits){i => Bool(false) } ++
    Array(Bool(negative))
  }
}
protected case class Value(value: BigDecimal) extends FloatValue {
  def negative: Boolean = value < 0
  // Default cutoff for formatting for scala Double is 1E7, so using the same here
  override def toString: String = {
    if (value.abs >= 1E7 || value.abs < 1E-7) value.bigDecimal.toEngineeringString
    else value.bigDecimal.toPlainString
  }

  def bits(fmt: FltFormat): Array[Bool] = FloatPoint.clamp(value, fmt) match {
    case Left(fv) => fv.bits(fmt)
    case Right((s,mantissa,exp)) =>
      Array.tabulate(fmt.sbits){i => Bool(mantissa.testBit(i)) } ++
      Array.tabulate(fmt.ebits){i => Bool(exp.testBit(i)) } ++
      Array(Bool(s))
  }
}
object FloatValue {
  def apply(x: Float): FloatValue = {
    if (x.isNaN) NaN
    else if (x.isInfinity) Inf(negative = x < 0)
    else if (x == 0.0f) Zero(negative = 1 / x < 0)
    else Value(BigDecimal(x.toDouble))
  }
  def apply(x: Double): FloatValue = {
    if (x.isNaN) NaN
    else if (x.isInfinity) Inf(negative = x < 0)
    else if (x == 0.0) Zero(negative = 1 / x < 0)
    else Value(BigDecimal(x))
  }
  def apply(x: BigDecimal): FloatValue = Value(x)
}

class FloatPoint(val value: FloatValue, val valid: Boolean, val fmt: FltFormat) {
  // All operations assume that both the left and right hand side have the same fixed point format
  def +(that: FloatPoint) = FloatPoint.clamped(this.value + that.value, this.valid && that.valid, fmt)
  def -(that: FloatPoint) = FloatPoint.clamped(this.value - that.value, this.valid && that.valid, fmt)
  def *(that: FloatPoint) = FloatPoint.clamped(this.value * that.value, this.valid && that.valid, fmt)
  def /(that: FloatPoint) = FloatPoint.clamped(this.value / that.value, this.valid && that.valid, fmt)
  def %(that: FloatPoint) = FloatPoint.clamped(this.value % that.value, this.valid && that.valid, fmt)

  def <(that: FloatPoint)   = Bool(this.value < that.value, this.valid && that.valid)
  def <=(that: FloatPoint)  = Bool(this.value <= that.value, this.valid && that.valid)
  def >(that: FloatPoint)   = Bool(this.value > that.value, this.valid && that.valid)
  def >=(that: FloatPoint)  = Bool(this.value >= that.value, this.valid && that.valid)
  def !==(that: FloatPoint) = Bool(this.value != that.value, this.valid && that.valid)
  def ===(that: FloatPoint) = Bool(this.value == that.value, this.valid && that.valid)

  def toBigDecimal: BigDecimal = value match {
    case Zero(z) => BigDecimal(0)
    case Value(v) => v
    case v => throw new Exception(s"Cannot convert $v to BigDecimal")
  }
  def toDouble: Double = value match {
    case NaN       => Double.NaN
    case Inf(neg)  => if (neg) Double.NegativeInfinity else Double.PositiveInfinity
    case Zero(neg) => if (neg) -0.0 else 0.0
    case Value(v)  => v.toDouble
  }
  def toFloat: Float = value match {
    case NaN       => Float.NaN
    case Inf(neg)  => if (neg) Float.NegativeInfinity else Float.PositiveInfinity
    case Zero(neg) => if (neg) -0.0f else 0.0f
    case Value(v)  => v.toFloat
  }

  def bits: Array[Bool] = value.bits(fmt)
  def bitString: String = "0b" + bits.reverse.map{x =>
    if (x.valid && x.value) "1" else if (x.valid) "0" else "X"
  }.mkString("")

  def isNaN: Boolean = value == NaN
  def isPositiveInfinity: Boolean = value == Inf(false)
  def isNegativeInfinity: Boolean = value == Inf(true)
  def isPosZero: Boolean = value == Zero(true)
  def isNegZero: Boolean = value == Zero(false)
  def isSubnormal: Boolean = value match {
    case Value(v) => FloatPoint.clamp(v, fmt) match {
      case Right((s,m,e)) => e == 0
      case _ => false
    }
    case _ => false
  }

  override def toString: String = if (valid) value.toString else "X"
}

object FloatPoint {
  def apply(x: Byte, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)
  def apply(x: Short, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)
  def apply(x: Int, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)
  def apply(x: Long, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)

  def apply(x: Float, fmt: FltFormat): FloatPoint = FloatPoint.clamped(FloatValue(x), valid=true, fmt)
  def apply(x: Double, fmt: FltFormat): FloatPoint = FloatPoint.clamped(FloatValue(x), valid=true, fmt)
  def apply(x: BigDecimal, fmt: FltFormat): FloatPoint = FloatPoint.clamped(FloatValue(x), valid=true, fmt)

  /**
    * Stolen from https://stackoverflow.com/questions/6827516/logarithm-for-biginteger/7982137#7982137
    */
  private val LOG2 = Math.log(2.0)
  def log2BigInt(value: BigInt): Double = {
    val blex = value.bitLength - 1022 // any value in 60 .. 1023 is ok
    val shifted = if (blex > 0) value >> blex else value
    val res = Math.log(shifted.doubleValue) / LOG2
    if (blex > 0) res + blex else res
  }

  def log2BigDecimal(value: BigDecimal): Double = {
    if (value.abs >= 1) {
      val fracPart = value % 1
      val intPart = if (value < 0) value + fracPart else value - fracPart
      log2BigInt(intPart.toBigInt)
    }
    else {
      -log2BigInt((1/value).toBigInt)
    }
  }

  def clamp(value: BigDecimal, fmt: FltFormat): Either[FloatValue, (Boolean,BigInt,BigInt)] = {
    if (value == 0) {
      Left(Zero(negative = false))
    }
    else {
      val y = Math.round(log2BigDecimal(value.abs)).toInt // Note: NOT floor or ceil
      val x = value.abs / BigDecimal(2).pow(y)
      println(s"exp: $y [${fmt.MIN_E} : ${fmt.MAX_E}, sub: ${fmt.SUB_E}]")
      println(s"man: $x")
      if (y > fmt.MAX_E) {
        Left(Inf(negative = value < 0))
      }
      else if (y >= fmt.MIN_E) {
        val mantissaP1 = ((x - 1) * BigDecimal(2).pow(fmt.sbits + 1)).toBigInt
        val mantissa = (mantissaP1 + (if (mantissaP1.testBit(0)) 1 else 0)) >> 1
        val expPart = y + fmt.bias
        Right((value < 0, mantissa, expPart))
      }
      else if (y < fmt.MIN_E && y >= fmt.SUB_E) {
        val mantissa = (x * BigDecimal(2).pow(fmt.sbits + 1)).toBigInt
        val expBits = BigInt(0)
        val shift = (fmt.MIN_E - y + 1).toInt
        val shiftedMantissa = (mantissa >> shift) + (if (mantissa.testBit(shift-1)) 1 else 0)
        println(s"mantissa: " + Array.tabulate(fmt.sbits+1){i => mantissa.testBit(i) }.map{x => if (x) 1 else 0}.reverse.mkString(""))
        println(s"mantissa: " + Array.tabulate(fmt.sbits+1){i => shiftedMantissa.testBit(i) }.map{x => if (x) 1 else 0}.reverse.mkString(""))
        println(s"shift: $shift")
        Right((value < 0, shiftedMantissa, expBits))
      }
      else {
        Left(Zero(negative = value < 0))
      }
    }
  }
  def convertBackToValue(m: Either[FloatValue, (Boolean,BigInt,BigInt)], fmt: FltFormat): FloatValue = m match {
    case Right((s,m,e)) =>
      if (e > 0) {
        val y = e.toInt - fmt.bias.toInt
        val x = BigDecimal(m) / BigDecimal(2).pow(fmt.sbits) + 1 //+ (if (e == 1) 0 else 1)
        println(s"x: $x")
        println(s"y: $y")
        val sign = if (s) -1 else 1
        Value(x * BigDecimal(2).pow(y) * sign)
      }
      else {
        val x = BigDecimal(m) / BigDecimal(2).pow(fmt.sbits - 1)
        val y = BigDecimal(2).pow(fmt.MIN_E.toInt - 1)
        val sign = if (s) -1 else 1
        Value(sign * x * y)
      }
    case Left(value) => value
  }

  def clamped(value: FloatValue, valid: Boolean, fmt: FltFormat): FloatPoint = value match {
    case NaN | _: Inf | _:Zero => new FloatPoint(value, valid, fmt)
    case Value(v) =>
      val m = clamp(v, fmt)
      val actualValue = convertBackToValue(m, fmt)
      new FloatPoint(actualValue, valid, fmt)
  }
}

def log2(x: BigDecimal) = Math.log(x.toDouble) / Math.log(2)

val q = Float.NaN //* 512.23231238123//* Math.pow(2, 51) * 1.1
val DoubleFmt = FltFormat(52, 11)
val FloatFmt = FltFormat(23, 8)
val m = FloatPoint(q, FloatFmt)

def bitString(x: Double) = {
  val str = java.lang.Long.toBinaryString(java.lang.Double.doubleToRawLongBits(x))
  "0b" + ("0" * (64 - str.length)) + str
}
def bitString(x: Float) = {
  val str = java.lang.Integer.toBinaryString(java.lang.Float.floatToIntBits(x))
  "0b" + ("0" * (32 - str.length)) + str
}
def annotate(x: String) = {
  x.slice(0,3) + "|" + x.slice(3,15) + "|" + x.slice(15,x.length)
}


annotate(m.bitString)
annotate(bitString(q))

m.isSubnormal
