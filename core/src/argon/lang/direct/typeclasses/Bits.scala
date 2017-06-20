package argon.lang.direct
package typeclasses

import forge._

trait BitsExp {
  def bits[T:Bits] = implicitly[Bits[T]]
  def mbits[T,R](s: Bits[T]): Bits[R] = s.asInstanceOf[Bits[R]]
}

trait BitsApi {
  @api def zero[T:Bits]: T = implicitly[Bits[T]].zero
  @api def one[T:Bits]: T = implicitly[Bits[T]].one
  @api def random[T:Bits]: T = implicitly[Bits[T]].random(None)
  @api def random[T:Bits](max: T): T = implicitly[Bits[T]].random(Some(max))
}