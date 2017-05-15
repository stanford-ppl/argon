package argon.lang

import argon._
import argon.typeclasses._
import forge._

case class Tup2[A:Type,B:Type](s: Exp[Tup2[A,B]]) extends Struct[Tup2[A,B]] {
  @api def _1: A = field[A]("_1")
  @api def _2: B = field[B]("_2")
}

case class Tup2Type[A,B](m1: Type[A], m2: Type[B]) extends StructType[Tup2[A,B]] with CanBits[Tup2[A,B]] {
  override def wrapped(x: Exp[Tup2[A,B]]): Tup2[A,B] = Tup2(x)(m1,m2)
  override def typeArguments = List(m1, m2)
  override def stagedClass = classOf[Tup2[A,B]]

  override def fields = List("_1" -> m1, "_2" -> m2)
  protected def getBits(children: Seq[Type[_]]) = (m1,m2) match {
    case (Bits(b1),Bits(b2)) => Some(new Tup2Bits[A,B]()(m1,b1,m2,b2))
    case _ => None
  }
}

class Tup2Bits[A:Type:Bits,B:Type:Bits] extends Bits[Tup2[A,B]] {
  @api def zero: Tup2[A,B] = Tup2.pack(bits[A].zero, bits[B].zero)
  @api def one: Tup2[A,B] = Tup2.pack(bits[A].one, bits[B].one)
  @api def random(max: Option[Tup2[A, B]]): Tup2[A,B] = {
    Tup2.pack(bits[A].random(max.map(_._1)), bits[B].random(max.map(_._2)))
  }
  @api def length: Int = bits[A].length + bits[B].length
}

class Tup2Arith[A:Type:Arith,B:Type:Arith] extends Arith[Tup2[A,B]] {
  @api def negate(x: Tup2[A, B])                = Tup2.pack(arith[A].negate(x._1),      arith[B].negate(x._2))
  @api def plus(x: Tup2[A, B], y: Tup2[A, B])   = Tup2.pack(arith[A].plus(x._1,y._1),   arith[B].plus(x._2,y._2))
  @api def minus(x: Tup2[A, B], y: Tup2[A, B])  = Tup2.pack(arith[A].minus(x._1,y._1),  arith[B].minus(x._2,y._2))
  @api def times(x: Tup2[A, B], y: Tup2[A, B])  = Tup2.pack(arith[A].times(x._1,y._1),  arith[B].times(x._2,y._2))
  @api def divide(x: Tup2[A, B], y: Tup2[A, B]) = Tup2.pack(arith[A].divide(x._1,y._1), arith[B].divide(x._2,y._2))
}

object Tup2 {
  import Struct._
  @internal def pack[A:Type,B:Type](a: A, b: B): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> a.s, "_2" -> b.s)
  @internal def pack[A:Type,B:Type](t: (A, B)): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> t._1.s, "_2" -> t._2.s)
}


trait Tup2Exp {
  /** Static methods **/
  @api def pack[A:Type,B:Type](a: A, b: B): Tup2[A,B] = Tup2.pack(a,b)
  @api def pack[A:Type,B:Type](t: (A, B)): Tup2[A,B] = Tup2.pack(t)
  @api def unpack[A:Type,B:Type](t: Tup2[A,B]): (A,B) = (t._1, t._2)


  /** Type classes **/
  implicit def tup2IsStaged[A:Type,B:Type]: StructType[Tup2[A,B]] = Tup2Type(typ[A],typ[B])
  implicit def tup2CanBits[A:Type:Bits,B:Type:Bits]: Bits[Tup2[A,B]] = new Tup2Bits[A,B]
  implicit def tup2CanArith[A:Type:Arith,B:Type:Arith]: Arith[Tup2[A,B]] = new Tup2Arith[A,B]
}

