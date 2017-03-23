package argon.ops

import argon.core.Staging
import argon.typeclasses._

trait TupleApi extends TupleExp with StructApi with BitsApi with NumApi {
  this: TextApi =>
}

trait TupleExp extends Staging with StructExp with NumExp with BitsExp {
  this: TextExp =>

  /** Infix methods **/
  case class Tup2[A <: StageAny[A] : Staged,B <: StageAny[B] : Staged](s: Exp[Tup2[A,B]]) extends StructApi[Tup2[A,B]] with StageAny[Tup2[A,B]] {
    def _1(implicit ctx: SrcCtx): A = field[A]("_1")
    def _2(implicit ctx: SrcCtx): B = field[B]("_2")
  }

  /** Direct methods **/
  def pack[A <: StageAny[A] : Staged,B <: StageAny[B] : Staged](a: A, b: B)(implicit ctx: SrcCtx): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> a.s, "_2" -> b.s)
  def pack[A <: StageAny[A] : Staged,B <: StageAny[B] : Staged](t: (A, B))(implicit ctx: SrcCtx): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> t._1.s, "_2" -> t._2.s)
  def unpack[A <: StageAny[A] : Staged,B <: StageAny[B] : Staged](t: Tup2[A,B])(implicit ctx: SrcCtx): (A,B) = (t._1, t._2)

  /** Virtualized methods **/
  def infix_toString[A <: StageAny[A] : Staged,B <: StageAny[B] : Staged](x: Tup2[A,B])(implicit ctx: SrcCtx): Text = "Tup2(" + textify(x._1) + ", " + textify(x._2) + ")"

  /** Type classes **/
  // --- Staged
  case class Tup2Type[A <: StageAny[A] , B <: StageAny[B]](m1: Staged[A], m2: Staged[B]) extends StructType[Tup2[A,B]] {
    override def wrapped(x: Exp[Tup2[A,B]]): Tup2[A,B] = Tup2(x)(m1,m2)
    override def typeArguments = List(m1, m2)
    override def stagedClass = classOf[Tup2[A,B]]

    override def fields = List("_1" -> m1, "_2" -> m2)
  }
  implicit def tup2Type[A <: StageAny[A] : Staged,B <: StageAny[B] : Staged]: StructType[Tup2[A,B]] = Tup2Type(ftyp[A],ftyp[B])

  // --- Bits
  class Tup2Bits[A <: StageAny[A] : Staged:Bits,B <: StageAny[B] : Staged:Bits] extends Bits[Tup2[A,B]] {
    override def zero(implicit ctx: SrcCtx): Tup2[A,B] = pack(bits[A].zero, bits[B].zero)
    override def one(implicit ctx: SrcCtx): Tup2[A,B] = pack(bits[A].one, bits[B].one)
    override def random(max: Option[Tup2[A, B]])(implicit ctx: SrcCtx): Tup2[A,B] = {
      pack(bits[A].random(max.map(_._1)), bits[B].random(max.map(_._2)))
    }
    override def length: Int = bits[A].length + bits[B].length
  }
  implicit def __tup2Bits[A <: StageAny[A] : Staged:Bits,B <: StageAny[B] : Staged:Bits]: Bits[Tup2[A,B]] = new Tup2Bits[A,B]

  override protected def bitsUnapply[T <: StageAny[T]](tp: Staged[T]): Option[Bits[T]] = tp match {
    case tp: Tup2Type[_,_] => (tp.m1,tp.m2) match {
      case (Bits(b1),Bits(b2)) => Some(new Tup2Bits()(tp.m1,b1,tp.m2,b2).asInstanceOf[Bits[T]])
      case _ => None
    }
    case _ => super.bitsUnapply(tp)
  }

  // --- Num
  class Tup2Arith[A <: StageAny[A] : Staged:Arith,B <: StageAny[B] : Staged:Arith] extends Arith[Tup2[A,B]] {
    override def negate(x: Tup2[A, B])(implicit ctx: SrcCtx) = pack(arith[A].negate(x._1), arith[B].negate(x._2))
    override def plus(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx)   = pack(arith[A].plus(x._1,y._1),   arith[B].plus(x._2,y._2))
    override def minus(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx)  = pack(arith[A].minus(x._1,y._1),  arith[B].minus(x._2,y._2))
    override def times(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx)  = pack(arith[A].times(x._1,y._1),  arith[B].times(x._2,y._2))
    override def divide(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx) = pack(arith[A].divide(x._1,y._1), arith[B].divide(x._2,y._2))
  }
  implicit def __tup2Arith[A <: StageAny[A] : Staged:Arith,B <: StageAny[B] : Staged:Arith]: Arith[Tup2[A,B]] = new Tup2Arith[A,B]
}
