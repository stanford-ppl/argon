package argon.lang

import typeclasses._
import argon.internals._
import argon.nodes._
import forge._

case class Tuple2[A:Type,B:Type](s: Exp[Tuple2[A,B]]) extends Struct[Tuple2[A,B]] {
  //val mA: MetaAny[A] = typ[A].fake
  //val mB: MetaAny[B] = typ[B].fake
  override type Internal = scala.Tuple2[Any,Any]

  @api def _1: A = field[A]("_1")
  @api def _2: B = field[B]("_2")
}

object Tuple2 {
  @internal def pack[A:Type,B:Type](a: A, b: B): Tuple2[A,B] = Struct[Tuple2[A,B]]("_1" -> a.s, "_2" -> b.s)
  @internal def pack[A:Type,B:Type](t: (A, B)): Tuple2[A,B] = Struct[Tuple2[A,B]]("_1" -> t._1.s, "_2" -> t._2.s)

  /** Type classes **/
  implicit def tup2IsStaged[A:Type,B:Type]: StructType[Tuple2[A,B]] = Tuple2Type(typ[A],typ[B])
  implicit def tup2CanBits[A:Type:Bits,B:Type:Bits]: Bits[Tuple2[A,B]] = new Tuple2Bits[A,B]
  implicit def tup2CanArith[A:Type:Arith,B:Type:Arith]: Arith[Tuple2[A,B]] = new Tuple2Arith[A,B]
}


trait Tuple2Exp {
  /** Static methods **/
  @api def pack[A:Type,B:Type](a: A, b: B): Tuple2[A,B] = Tuple2.pack(a,b)
  @api def pack[A:Type,B:Type](t: (A, B)): Tuple2[A,B] = Tuple2.pack(t)
  @api def unpack[A:Type,B:Type](t: Tuple2[A,B]): (A,B) = (t._1, t._2)
}

