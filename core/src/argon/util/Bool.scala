package argon.util

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