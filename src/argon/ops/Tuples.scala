package argon.ops

trait TupleOps extends StructOps with NumOps {
  this: TextOps =>

  type Tup2[A,B]               <: Tup2Ops[A,B]
//  type Tup3[A,B,C]             <: Tup3Ops[A,B,C]
//  type Tup4[A,B,C,D]           <: Tup4Ops[A,B,C,D]
//  type Tup5[A,B,C,D,E]         <: Tup5Ops[A,B,C,D,E]
//  type Tup6[A,B,C,D,E,F]       <: Tup6Ops[A,B,C,D,E,F]
//  type Tup7[A,B,C,D,E,F,G]     <: Tup7Ops[A,B,C,D,E,F,G]
//  type Tup8[A,B,C,D,E,F,G,H]   <: Tup8Ops[A,B,C,D,E,F,G,H]
//  type Tup9[A,B,C,D,E,F,G,H,I] <: Tup9Ops[A,B,C,D,E,F,G,H,I]

  trait Tup2Ops[A,B] {
    def _1(implicit ctx: SrcCtx): A
    def _2(implicit ctx: SrcCtx): B
  }
//  trait Tup3Ops[A,B,C]             { def _1: A ; def _2: B ; def _3: C }
//  trait Tup4Ops[A,B,C,D]           { def _1: A ; def _2: B ; def _3: C ; def _4: D }
//  trait Tup5Ops[A,B,C,D,E]         { def _1: A ; def _2: B ; def _3: C ; def _4: D ; def _5: E }
//  trait Tup6Ops[A,B,C,D,E,F]       { def _1: A ; def _2: B ; def _3: C ; def _4: D ; def _5: E ; def _6: F ; }
//  trait Tup7Ops[A,B,C,D,E,F,G]     { def _1: A ; def _2: B ; def _3: C ; def _4: D ; def _5: E ; def _6: F ; def _7: G }
//  trait Tup8Ops[A,B,C,D,E,F,G,H]   { def _1: A ; def _2: B ; def _3: C ; def _4: D ; def _5: E ; def _6: F ; def _7: G ; def _8: H }
//  trait Tup9Ops[A,B,C,D,E,F,G,H,I] { def _1: A ; def _2: B ; def _3: C ; def _4: D ; def _5: E ; def _6: F ; def _7: G ; def _8: H ; def _9: I }
}

trait TupleApi extends TupleOps with StructApi with NumApi { this: TextApi => }


trait TupleLowestPriorityImplicits { this: TupleExp =>
  implicit def stagedTup2[A:Staged,B:Staged]: StructType[Tup2[A,B]] = new Tup2Type[A,B](typ[A],typ[B])
}

trait TupleLowPriorityImplicits extends TupleLowestPriorityImplicits { this: TupleExp =>
  implicit def bitsTup2[A:Bits,B:Bits]: Bits[Tup2[A,B]] = new Tup2Bits[A,B](bits[A],bits[B])
}

trait TupleExp extends TupleOps with TupleLowPriorityImplicits with StructExp with NumExp {
  this: TextExp =>

  /** API **/
  case class Tup2[A:Staged,B:Staged](s: Exp[Tup2[A,B]]) extends StructApi[Tup2[A,B]] with Tup2Ops[A,B] {
    def _1(implicit ctx: SrcCtx): A = field[A]("_1")
    def _2(implicit ctx: SrcCtx): B = field[B]("_2")
  }

  def pack[A:Staged,B:Staged](a: A, b: B)(implicit ctx: SrcCtx): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> a.s, "_2" -> b.s)
  def pack[A:Staged,B:Staged](t: (A, B))(implicit ctx: SrcCtx): Tup2[A,B] = struct[Tup2[A,B]]("_1" -> t._1.s, "_2" -> t._2.s)
  def unpack[A:Staged,B:Staged](t: Tup2[A,B])(implicit ctx: SrcCtx): (A,B) = (t._1, t._2)

  /** Staged Types **/
  class Tup2Type[A,B](val m1: Staged[A], val m2: Staged[B]) extends StructType[Tup2[A,B]] {
    private implicit def mA = m1 ; private implicit def mB = m2 // This is annoying

    override def wrapped(x: Exp[Tup2[A,B]]): Tup2[A,B] = Tup2(x)(m1,m2)
    override def unwrapped(x: Tup2[A, B]) = x.s
    override def typeArguments = List(m1, m2)
    override def stagedClass = classOf[Tup2[A,B]]
    override def isPrimitive = true

    override def fields = List("_1" -> m1, "_2" -> m2)

    override def equals(x: Any) = x match {
      case that: Tup2Type[_,_] => this.m1 == that.m1 && this.m2 == that.m2
      case _ => false
    }
    override def hashCode() = m1.## + m2.##
  }

  class Tup2Bits[A,B](override val m1: Bits[A], override val m2: Bits[B]) extends Tup2Type[A,B](m1,m2) with Bits[Tup2[A,B]] {
    private implicit def mA = m1 ; private implicit def mB = m2 // This is annoying

    override def random(max: Option[Tup2[A, B]])(implicit ctx: SrcCtx): Tup2[A,B] = {
      pack(m1.random(max.map(_._1)),m2.random(max.map(_._2)))
    }
    override def zero(implicit ctx: SrcCtx): Tup2[A,B] = pack(m1.zero,m2.zero)
    override def one(implicit ctx: SrcCtx): Tup2[A,B] = pack(m1.one, m2.one)
    override def length: Int = m1.length + m2.length
  }

  class Tup2Arith[A,B](override val m1: Arith[A], override val m2: Arith[B]) extends Tup2Bits[A,B](m1,m2) with Arith[Tup2[A,B]] {
    private implicit def mA = m1 ; private implicit def mB = m2 // This is annoying

    override def negate(x: Tup2[A, B])(implicit ctx: SrcCtx) = pack(m1.negate(x._1),m2.negate(x._2))
    override def plus(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx) = pack(m1.plus(x._1,y._1),m2.plus(x._2,y._2))
    override def minus(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx) = pack(m1.minus(x._1,y._1),m2.minus(x._2,y._2))
    override def times(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx) = pack(m1.times(x._1,y._1),m2.times(x._2,y._2))
    override def divide(x: Tup2[A, B], y: Tup2[A, B])(implicit ctx: SrcCtx) = pack(m1.divide(x._1,y._1),m2.divide(x._2,y._2))
  }

  implicit def arithTup2[A:Arith,B:Arith]: Arith[Tup2[A,B]] = new Tup2Arith[A,B](arith[A],arith[B])


  /** Internals **/


}
