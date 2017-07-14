package argon.util

case class FltFormat(sbits: Int, ebits: Int) {
  lazy val bias: BigInt = BigInt(2).pow(ebits - 1) - 1
  lazy val MIN_E: BigInt = -bias + 1  // Represented as all 0s
  lazy val MAX_E: BigInt = bias       // Represented as all 1s
}

protected sealed abstract class FloatValue {
  def negative: Boolean
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
}
protected case class Inf(negative: Boolean) extends FloatValue {
  override def toString: String = if (negative) "-Inf" else "Inf"
}
protected case class Zero(negative: Boolean) extends FloatValue {
  override def toString: String = if (negative) "-0.0" else "0.0"
}
protected case class Value(value: BigDecimal) extends FloatValue {
  def negative: Boolean = value < 0
  // Default cutoff for formatting for scala Double is 1E7, so using the same here
  override def toString: String = if (value.abs >= 1E7) value.bigDecimal.toEngineeringString
                                  else value.bigDecimal.toPlainString
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
}

object FloatPoint {
  def apply(x: Byte, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)
  def apply(x: Short, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)
  def apply(x: Int, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)
  def apply(x: Long, fmt: FltFormat): FloatPoint = FloatPoint.clamped(Value(BigDecimal(x)), valid=true, fmt)

  def apply(x: Float, fmt: FltFormat): FloatPoint = FloatPoint.clamped(FloatValue(x), valid=true, fmt)
  def apply(x: Double, fmt: FltFormat): FloatPoint = FloatPoint.clamped(FloatValue(x), valid=true, fmt)

  def clamped(value: FloatValue, valid: Boolean, fmt: FltFormat): FloatPoint = value match {
    case NaN | _: Inf | _:Zero => new FloatPoint(value, valid, fmt)
    case b: Value =>
      // TODO
      new FloatPoint(value, valid, fmt)
  }
}
