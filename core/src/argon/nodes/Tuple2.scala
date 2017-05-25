package argon.nodes

import argon.core.compiler._
import argon.lang.compiler._
import forge._

case class Tuple2Type[A,B](m1: Type[A], m2: Type[B]) extends StructType[MTuple2[A,B]] with CanBits[MTuple2[A,B]] {
  override def wrapped(x: Exp[MTuple2[A,B]]): MTuple2[A,B] = new MTuple2[A,B](x)(m1,m2)
  override def typeArguments = List(m1, m2)
  override def stagedClass = classOf[MTuple2[A,B]]

  override def fields = List("_1" -> m1, "_2" -> m2)
  protected def getBits(children: Seq[Type[_]]) = (m1,m2) match {
    case (Bits(b1),Bits(b2)) => Some(new Tuple2Bits[A,B]()(m1,b1,m2,b2))
    case _ => None
  }
}

class Tuple2Bits[A:Type:Bits,B:Type:Bits] extends Bits[MTuple2[A,B]] {
  @api def zero: MTuple2[A,B] = MTuple2.pack(bits[A].zero, bits[B].zero)
  @api def one: MTuple2[A,B] = MTuple2.pack(bits[A].one, bits[B].one)
  @api def random(max: Option[MTuple2[A, B]]): MTuple2[A,B] = {
    MTuple2.pack(bits[A].random(max.map(_._1)), bits[B].random(max.map(_._2)))
  }
  def length: Int = bits[A].length + bits[B].length
}

class Tuple2Arith[A:Type:Arith,B:Type:Arith] extends Arith[MTuple2[A,B]] {
  @api def negate(x: MTuple2[A, B])                   = MTuple2.pack(arith[A].negate(x._1),      arith[B].negate(x._2))
  @api def plus(x: MTuple2[A, B], y: MTuple2[A, B])   = MTuple2.pack(arith[A].plus(x._1,y._1),   arith[B].plus(x._2,y._2))
  @api def minus(x: MTuple2[A, B], y: MTuple2[A, B])  = MTuple2.pack(arith[A].minus(x._1,y._1),  arith[B].minus(x._2,y._2))
  @api def times(x: MTuple2[A, B], y: MTuple2[A, B])  = MTuple2.pack(arith[A].times(x._1,y._1),  arith[B].times(x._2,y._2))
  @api def divide(x: MTuple2[A, B], y: MTuple2[A, B]) = MTuple2.pack(arith[A].divide(x._1,y._1), arith[B].divide(x._2,y._2))
}
