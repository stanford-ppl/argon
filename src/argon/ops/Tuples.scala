package argon.ops

import argon.core.Staging
import argon.typeclasses._
import forge._



trait TupleApi extends TupleExp with StructApi with BitsApi with ArithApi {
  this: TextApi =>
}

trait TupleExp extends Staging with StructExp with ArithExp with BitsExp {
  this: TextExp =>

  /** Infix methods **/
  case class Tup2[A:Meta,B:Meta](s: Exp[Tup2[A,B]]) extends MetaStruct[Tup2[A,B]] {
    @api def _1: A = field[A]("_1")
    @api def _2: B = field[B]("_2")
  }

  /** Direct methods **/
  @api def pack[A:Meta,B:Meta](a: A, b: B): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> a.s, "_2" -> b.s)
  @api def pack[A:Meta,B:Meta](t: (A, B)): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> t._1.s, "_2" -> t._2.s)
  @api def unpack[A:Meta,B:Meta](t: Tup2[A,B]): (A,B) = (t._1, t._2)

  /** Type classes **/
  // --- Staged
  case class Tup2Type[A,B](m1: Meta[A], m2: Meta[B]) extends StructType[Tup2[A,B]] with CanBits[Tup2[A,B]] {
    override def wrapped(x: Exp[Tup2[A,B]]): Tup2[A,B] = Tup2(x)(m1,m2)
    override def typeArguments = List(m1, m2)
    override def stagedClass = classOf[Tup2[A,B]]

    override def fields = List("_1" -> m1, "_2" -> m2)
    protected def getBits(children: Seq[Type[_]]) = (m1,m2) match {
      case (Bits(b1),Bits(b2)) => Some(__tup2Bits(m1,b1,m2,b2))
      case _ => None
    }
  }
  implicit def tup2Type[A:Meta,B:Meta]: StructType[Tup2[A,B]] = Tup2Type(meta[A],meta[B])

  // --- Bits
  class Tup2Bits[A:Meta:Bits,B:Meta:Bits] extends Bits[Tup2[A,B]] {
    override def zero(implicit ctx: SrcCtx): Tup2[A,B] = pack(bits[A].zero, bits[B].zero)
    override def one(implicit ctx: SrcCtx): Tup2[A,B] = pack(bits[A].one, bits[B].one)
    override def random(max: Option[Tup2[A, B]])(implicit ctx: SrcCtx): Tup2[A,B] = {
      pack(bits[A].random(max.map(_._1)), bits[B].random(max.map(_._2)))
    }
    override def length: Int = bits[A].length + bits[B].length
  }
  implicit def __tup2Bits[A:Meta:Bits,B:Meta:Bits]: Bits[Tup2[A,B]] = new Tup2Bits[A,B]

  // --- Num
  class Tup2Arith[A:Meta:Arith,B:Meta:Arith] extends Arith[Tup2[A,B]] {
    override def negate(x: Tup2[A, B])(implicit ctx: SrcCtx) = pack(arith[A].negate(x._1), arith[B].negate(x._2))
    override def plus(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx)   = pack(arith[A].plus(x._1,y._1),   arith[B].plus(x._2,y._2))
    override def minus(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx)  = pack(arith[A].minus(x._1,y._1),  arith[B].minus(x._2,y._2))
    override def times(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx)  = pack(arith[A].times(x._1,y._1),  arith[B].times(x._2,y._2))
    override def divide(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx) = pack(arith[A].divide(x._1,y._1), arith[B].divide(x._2,y._2))
  }
  implicit def __tup2Arith[A:Meta:Arith,B:Meta:Arith]: Arith[Tup2[A,B]] = new Tup2Arith[A,B]
}
