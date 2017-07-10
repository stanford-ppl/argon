package argon.lang.direct
package typeclasses

trait ArithExp {
  def arith[T:Arith] = implicitly[Arith[T]]
}