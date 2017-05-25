package argon.nodes

import argon.core.compiler._
import argon.lang.compiler._

case class Func1Type[A,R](a: Type[A], r: Type[R]) extends Type[Func1[A,R]] {
  override def wrapped(x: Exp[Func1[A,R]]) = new Func1(x)(a, r)
  override def unwrapped(x: Func1[A,R]) = x.s
  override def stagedClass = classOf[Func1[A,R]]
  override def typeArguments = List(a, r)
  override def isPrimitive: CBoolean = false
}
case class Func2Type[A,B,R](a: Type[A], b: Type[B], r: Type[R]) extends Type[Func2[A,B,R]] {
  override def wrapped(x: Exp[Func2[A,B,R]]) = new Func2(x)(a, b, r)
  override def unwrapped(x: Func2[A,B,R]) = x.s
  override def stagedClass = classOf[Func2[A,B,R]]
  override def typeArguments = List(a, b, r)
  override def isPrimitive: CBoolean = false
}
case class Func3Type[A,B,C,R](a: Type[A], b: Type[B], c: Type[C], r: Type[R]) extends Type[Func3[A,B,C,R]] {
  override def wrapped(x: Exp[Func3[A,B,C,R]]) = new Func3(x)(a, b, c, r)
  override def unwrapped(x: Func3[A,B,C,R]) = x.s
  override def stagedClass = classOf[Func3[A,B,C,R]]
  override def typeArguments = List(a, b, c, r)
  override def isPrimitive: CBoolean = false
}
case class Func4Type[A,B,C,D,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], r: Type[R]) extends Type[Func4[A,B,C,D,R]] {
  override def wrapped(x: Exp[Func4[A,B,C,D,R]]) = new Func4(x)(a, b, c, d, r)
  override def unwrapped(x: Func4[A,B,C,D,R]) = x.s
  override def stagedClass = classOf[Func4[A,B,C,D,R]]
  override def typeArguments = List(a, b, c, d, r)
  override def isPrimitive: CBoolean = false
}
case class Func5Type[A,B,C,D,E,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], e: Type[E], r: Type[R]) extends Type[Func5[A,B,C,D,E,R]] {
  override def wrapped(x: Exp[Func5[A,B,C,D,E,R]]) = new Func5(x)(a, b, c, d, e, r)
  override def unwrapped(x: Func5[A,B,C,D,E,R]) = x.s
  override def stagedClass = classOf[Func5[A,B,C,D,E,R]]
  override def typeArguments = List(a, b, c, d, e, r)
  override def isPrimitive: CBoolean = false
}
case class Func6Type[A,B,C,D,E,F,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], e: Type[E], f: Type[F], r: Type[R]) extends Type[Func6[A,B,C,D,E,F,R]] {
  override def wrapped(x: Exp[Func6[A,B,C,D,E,F,R]]) = new Func6(x)(a, b, c, d, e, f, r)
  override def unwrapped(x: Func6[A,B,C,D,E,F,R]) = x.s
  override def stagedClass = classOf[Func6[A,B,C,D,E,F,R]]
  override def typeArguments = List(a, b, c, d, e, f, r)
  override def isPrimitive: CBoolean = false
}
case class Func7Type[A,B,C,D,E,F,G,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], e: Type[E], f: Type[F], g: Type[G], r: Type[R]) extends Type[Func7[A,B,C,D,E,F,G,R]] {
  override def wrapped(x: Exp[Func7[A,B,C,D,E,F,G,R]]) = new Func7(x)(a, b, c, d, e, f, g, r)
  override def unwrapped(x: Func7[A,B,C,D,E,F,G,R]) = x.s
  override def stagedClass = classOf[Func7[A,B,C,D,E,F,G,R]]
  override def typeArguments = List(a, b, c, d, e, f, g, r)
  override def isPrimitive: CBoolean = false
}
case class Func8Type[A,B,C,D,E,F,G,H,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], e: Type[E], f: Type[F], g: Type[G], h: Type[H], r: Type[R]) extends Type[Func8[A,B,C,D,E,F,G,H,R]] {
  override def wrapped(x: Exp[Func8[A,B,C,D,E,F,G,H,R]]) = new Func8(x)(a, b, c, d, e, f, g, h, r)
  override def unwrapped(x: Func8[A,B,C,D,E,F,G,H,R]) = x.s
  override def stagedClass = classOf[Func8[A,B,C,D,E,F,G,H,R]]
  override def typeArguments = List(a, b, c, d, e, f, g, h, r)
  override def isPrimitive: CBoolean = false
}
case class Func9Type[A,B,C,D,E,F,G,H,I,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], e: Type[E], f: Type[F], g: Type[G], h: Type[H], i: Type[I], r: Type[R]) extends Type[Func9[A,B,C,D,E,F,G,H,I,R]] {
  override def wrapped(x: Exp[Func9[A,B,C,D,E,F,G,H,I,R]]) = new Func9(x)(a, b, c, d, e, f, g, h, i, r)
  override def unwrapped(x: Func9[A,B,C,D,E,F,G,H,I,R]) = x.s
  override def stagedClass = classOf[Func9[A,B,C,D,E,F,G,H,I,R]]
  override def typeArguments = List(a, b, c, d, e, f, g, h, i, r)
  override def isPrimitive: CBoolean = false
}
case class Func10Type[A,B,C,D,E,F,G,H,I,J,R](a: Type[A], b: Type[B], c: Type[C], d: Type[D], e: Type[E], f: Type[F], g: Type[G], h: Type[H], i: Type[I], j: Type[J], r: Type[R]) extends Type[Func10[A,B,C,D,E,F,G,H,I,J,R]] {
  override def wrapped(x: Exp[Func10[A,B,C,D,E,F,G,H,I,J,R]]) = new Func10(x)(a, b, c, d, e, f, g, h, i, j, r)
  override def unwrapped(x: Func10[A,B,C,D,E,F,G,H,I,J,R]) = x.s
  override def stagedClass = classOf[Func10[A,B,C,D,E,F,G,H,I,J,R]]
  override def typeArguments = List(a, b, c, d, e, f, g, h, i, j, r)
  override def isPrimitive: CBoolean = false
}


case class FunApply1[A:Type,R:Type](
  fun: Exp[Func1[A,R]],
  a: Exp[A]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply1(tx(fun),tx(a))
}
case class FunApply2[A:Type,B:Type,R:Type](
  fun: Exp[Func2[A,B,R]],
  a: Exp[A], b: Exp[B]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply2(tx(fun),tx(a), tx(b))
}
case class FunApply3[A:Type,B:Type,C:Type,R:Type](
  fun: Exp[Func3[A,B,C,R]],
  a: Exp[A], b: Exp[B], c: Exp[C]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply3(tx(fun),tx(a), tx(b), tx(c))
}
case class FunApply4[A:Type,B:Type,C:Type,D:Type,R:Type](
  fun: Exp[Func4[A,B,C,D,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply4(tx(fun),tx(a), tx(b), tx(c), tx(d))
}
case class FunApply5[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](
  fun: Exp[Func5[A,B,C,D,E,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply5(tx(fun),tx(a), tx(b), tx(c), tx(d), tx(e))
}
case class FunApply6[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](
  fun: Exp[Func6[A,B,C,D,E,F,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply6(tx(fun),tx(a), tx(b), tx(c), tx(d), tx(e), tx(f))
}
case class FunApply7[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](
  fun: Exp[Func7[A,B,C,D,E,F,G,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply7(tx(fun),tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g))
}
case class FunApply8[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](
  fun: Exp[Func8[A,B,C,D,E,F,G,H,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply8(tx(fun),tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), tx(h))
}
case class FunApply9[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](
  fun: Exp[Func9[A,B,C,D,E,F,G,H,I,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply9(tx(fun),tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), tx(h), tx(i))
}
case class FunApply10[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](
  fun: Exp[Func10[A,B,C,D,E,F,G,H,I,J,R]],
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I], j: Exp[J]
) extends Op[R] {
  def mirror(tx:Tx) = Func.apply10(tx(fun),tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), tx(h), tx(i), tx(j))
}

case class FunDecl1[A:Type,R:Type](
  a: Exp[A],
  block: Block[R]
) extends Op[Func1[A,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl1(tx(a), blk), blk.effects)(here)
  }
}
case class FunDecl2[A:Type,B:Type,R:Type](
  a: Exp[A], b: Exp[B],
  block: Block[R]
) extends Op[Func2[A,B,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl2(tx(a), tx(b), blk), blk.effects)(here)
  }
}
case class FunDecl3[A:Type,B:Type,C:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C],
  block: Block[R]
) extends Op[Func3[A,B,C,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl3(tx(a), tx(b), tx(c), blk), blk.effects)(here)
  }
}
case class FunDecl4[A:Type,B:Type,C:Type,D:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D],
  block: Block[R]
) extends Op[Func4[A,B,C,D,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl4(tx(a), tx(b), tx(c), tx(d), blk), blk.effects)(here)
  }
}
case class FunDecl5[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E],
  block: Block[R]
) extends Op[Func5[A,B,C,D,E,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl5(tx(a), tx(b), tx(c), tx(d), tx(e), blk), blk.effects)(here)
  }
}
case class FunDecl6[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F],
  block: Block[R]
) extends Op[Func6[A,B,C,D,E,F,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl6(tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), blk), blk.effects)(here)
  }
}
case class FunDecl7[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G],
  block: Block[R]
) extends Op[Func7[A,B,C,D,E,F,G,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl7(tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), blk), blk.effects)(here)
  }
}
case class FunDecl8[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H],
  block: Block[R]
) extends Op[Func8[A,B,C,D,E,F,G,H,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl8(tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), tx(h), blk), blk.effects)(here)
  }
}
case class FunDecl9[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I],
  block: Block[R]
) extends Op[Func9[A,B,C,D,E,F,G,H,I,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl9(tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), tx(h), tx(i), blk), blk.effects)(here)
  }
}
case class FunDecl10[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](
  a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I], j: Exp[J],
  block: Block[R]
) extends Op[Func10[A,B,C,D,E,F,G,H,I,J,R]] {
  def mirror(tx:Tx) = {
    val blk = tx.tx(block)
    stageEffectful(FunDecl10(tx(a), tx(b), tx(c), tx(d), tx(e), tx(f), tx(g), tx(h), tx(i), tx(j), blk), blk.effects)(here)
  }
}
