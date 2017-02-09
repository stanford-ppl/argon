package argon.typeclasses

import argon.ops.BoolExp

trait NumApi extends NumExp { this: BoolExp => }
trait NumExp extends ArithExp with BitsExp with OrderExp {
  this: BoolExp =>

  trait Num[T] extends Bits[T] with Arith[T] with Order[T]

  def num[T:Num] = implicitly[Num[T]]
}
