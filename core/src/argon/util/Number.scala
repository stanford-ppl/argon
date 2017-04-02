package argon.util

// TODO: BigDecimal has no support for IEEE floating point NaN, positive/negative infinity, or negative zero
case class Number(v: BigDecimal, isNaN: Boolean = false, isPosInf: Boolean = false, isNegInf: Boolean = false) {

}
