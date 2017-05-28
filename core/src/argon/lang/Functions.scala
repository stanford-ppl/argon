package argon.lang

import argon.core.compiler._
import argon.nodes._
import forge._
import org.virtualized.EmptyContext

case class Func1[A:Type,R:Type](s: Exp[Func1[A,R]]) extends MetaAny[Func1[A,R]] with Function2[A,State,R] {
  @api def ===(that: Func1[A,R]) = ???
  @api def =!=(that: Func1[A,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, state: State): R = applyArg(a)(state, EmptyContext)
  @api def applyArg(a: A)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply1(s, a.s))
}
case class Func2[A:Type,B:Type,R:Type](s: Exp[Func2[A,B,R]]) extends MetaAny[Func2[A,B,R]] with Function3[A,B,State,R] {
  @api def ===(that: Func2[A,B,R]) = ???
  @api def =!=(that: Func2[A,B,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, state: State): R = applyArg(a: A, b: B)(state, EmptyContext)
  @api def applyArg(a: A, b: B)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply2(s, a.s, b.s))
}
case class Func3[A:Type,B:Type,C:Type,R:Type](s: Exp[Func3[A,B,C,R]]) extends MetaAny[Func3[A,B,C,R]] with Function4[A,B,C,State,R]  {
  @api def ===(that: Func3[A,B,C,R]) = ???
  @api def =!=(that: Func3[A,B,C,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, state: State): R = applyArg(a: A, b: B, c: C)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply3(s, a.s, b.s, c.s))
}
case class Func4[A:Type,B:Type,C:Type,D:Type,R:Type](s: Exp[Func4[A,B,C,D,R]]) extends MetaAny[Func4[A,B,C,D,R]] with Function5[A,B,C,D,State,R]  {
  @api def ===(that: Func4[A,B,C,D,R]) = ???
  @api def =!=(that: Func4[A,B,C,D,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, state: State): R = applyArg(a: A, b: B, c: C, d: D)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply4(s, a.s, b.s, c.s, d.s))
}
case class Func5[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](s: Exp[Func5[A,B,C,D,E,R]]) extends MetaAny[Func5[A,B,C,D,E,R]] with Function6[A,B,C,D,E,State,R]  {
  @api def ===(that: Func5[A,B,C,D,E,R]) = ???
  @api def =!=(that: Func5[A,B,C,D,E,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, e: E, state: State): R = applyArg(a: A, b: B, c: C, d: D, e: E)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D, e: E)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply5(s, a.s, b.s, c.s, d.s, e.s))
}
case class Func6[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](s: Exp[Func6[A,B,C,D,E,F,R]]) extends MetaAny[Func6[A,B,C,D,E,F,R]] with Function7[A,B,C,D,E,F,State,R]  {
  @api def ===(that: Func6[A,B,C,D,E,F,R]) = ???
  @api def =!=(that: Func6[A,B,C,D,E,F,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, state: State): R = applyArg(a: A, b: B, c: C, d: D, e: E, f: F)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D, e: E, f: F)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply6(s, a.s, b.s, c.s, d.s, e.s, f.s))
}
case class Func7[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](s: Exp[Func7[A,B,C,D,E,F,G,R]]) extends MetaAny[Func7[A,B,C,D,E,F,G,R]] with Function8[A,B,C,D,E,F,G,State,R]  {
  @api def ===(that: Func7[A,B,C,D,E,F,G,R]) = ???
  @api def =!=(that: Func7[A,B,C,D,E,F,G,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, state: State): R = applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply7(s, a.s, b.s, c.s, d.s, e.s, f.s, g.s))
}
case class Func8[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](s: Exp[Func8[A,B,C,D,E,F,G,H,R]]) extends MetaAny[Func8[A,B,C,D,E,F,G,H,R]] with Function9[A,B,C,D,E,F,G,H,State,R]  {
  @api def ===(that: Func8[A,B,C,D,E,F,G,H,R]) = ???
  @api def =!=(that: Func8[A,B,C,D,E,F,G,H,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, state: State): R = applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply8(s, a.s, b.s, c.s, d.s, e.s, f.s, g.s, h.s))
}
case class Func9[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](s: Exp[Func9[A,B,C,D,E,F,G,H,I,R]]) extends MetaAny[Func9[A,B,C,D,E,F,G,H,I,R]] with Function10[A,B,C,D,E,F,G,H,I,State,R]  {
  @api def ===(that: Func9[A,B,C,D,E,F,G,H,I,R]) = ???
  @api def =!=(that: Func9[A,B,C,D,E,F,G,H,I,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, state: State): R = applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply9(s, a.s, b.s, c.s, d.s, e.s, f.s, g.s, h.s, i.s))
}
case class Func10[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](s: Exp[Func10[A,B,C,D,E,F,G,H,I,J,R]]) extends MetaAny[Func10[A,B,C,D,E,F,G,H,I,J,R]] with Function11[A,B,C,D,E,F,G,H,I,J,State,R]  {
  @api def ===(that: Func10[A,B,C,D,E,F,G,H,I,J,R]) = ???
  @api def =!=(that: Func10[A,B,C,D,E,F,G,H,I,J,R]) = ???
  @api def toText = String.ify(this)

  def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, state: State): R = applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(state, EmptyContext)
  @api def applyArg(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit state: State, ctx: SrcCtx): R = wrap(Func.apply10(s, a.s, b.s, c.s, d.s, e.s, f.s, g.s, h.s, i.s, j.s))
}

object Func {
  @internal def apply1[A:Type,R:Type](fun: Exp[Func1[A,R]], a: Exp[A]): Exp[R] = stage(FunApply1(fun, a: Exp[A]))(ctx)
  @internal def apply2[A:Type,B:Type,R:Type](fun: Exp[Func2[A,B,R]], a: Exp[A], b: Exp[B]): Exp[R] = stage(FunApply2(fun, a: Exp[A], b: Exp[B]))(ctx)
  @internal def apply3[A:Type,B:Type,C:Type,R:Type](fun: Exp[Func3[A,B,C,R]], a: Exp[A], b: Exp[B], c: Exp[C]): Exp[R] = stage(FunApply3(fun, a: Exp[A], b: Exp[B], c: Exp[C]))(ctx)
  @internal def apply4[A:Type,B:Type,C:Type,D:Type,R:Type](fun: Exp[Func4[A,B,C,D,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]): Exp[R] = stage(FunApply4(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]))(ctx)
  @internal def apply5[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](fun: Exp[Func5[A,B,C,D,E,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E]): Exp[R] = stage(FunApply5(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E]))(ctx)
  @internal def apply6[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](fun: Exp[Func6[A,B,C,D,E,F,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F]): Exp[R] = stage(FunApply6(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F]))(ctx)
  @internal def apply7[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](fun: Exp[Func7[A,B,C,D,E,F,G,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G]): Exp[R] = stage(FunApply7(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G]))(ctx)
  @internal def apply8[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](fun: Exp[Func8[A,B,C,D,E,F,G,H,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H]): Exp[R] = stage(FunApply8(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H]))(ctx)
  @internal def apply9[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](fun: Exp[Func9[A,B,C,D,E,F,G,H,I,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I]): Exp[R] = stage(FunApply9(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I]))(ctx)
  @internal def apply10[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](fun: Exp[Func10[A,B,C,D,E,F,G,H,I,J,R]], a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I], j: Exp[J]): Exp[R] = stage(FunApply10(fun, a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F], g: Exp[G], h: Exp[H], i: Exp[I], j: Exp[J]))(ctx)

  @internal def decl1[A:Type,R:Type](func: Function1[A,R]): Func1[A,R] = {
    val a = fresh[A]
    val blk = stageBlock{ func(wrap(a)).s }
    wrap{ stageEffectful(FunDecl1(a, blk), blk.effects)(ctx) }
  }
  @internal def decl2[A:Type,B:Type,R:Type](func: Function2[A,B,R]): Func2[A,B,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val blk = stageBlock{ func(wrap(a), wrap(b)).s }
    wrap{ stageEffectful(FunDecl2(a, b, blk), blk.effects)(ctx) }
  }
  @internal def decl3[A:Type,B:Type,C:Type,R:Type](func: Function3[A,B,C,R]): Func3[A,B,C,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c)).s }
    wrap{ stageEffectful(FunDecl3(a, b, c, blk), blk.effects)(ctx) }
  }
  @internal def decl4[A:Type,B:Type,C:Type,D:Type,R:Type](func: Function4[A,B,C,D,R]): Func4[A,B,C,D,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d)).s }
    wrap{ stageEffectful(FunDecl4(a, b, c, d, blk), blk.effects)(ctx) }
  }
  @internal def decl5[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](func: Function5[A,B,C,D,E,R]): Func5[A,B,C,D,E,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val e = fresh[E]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d), wrap(e)).s }
    wrap{ stageEffectful(FunDecl5(a, b, c, d, e, blk), blk.effects)(ctx) }
  }
  @internal def decl6[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](func: Function6[A,B,C,D,E,F,R]): Func6[A,B,C,D,E,F,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val e = fresh[E]
    val f = fresh[F]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d), wrap(e), wrap(f)).s }
    wrap{ stageEffectful(FunDecl6(a, b, c, d, e, f, blk), blk.effects)(ctx) }
  }
  @internal def decl7[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](func: Function7[A,B,C,D,E,F,G,R]): Func7[A,B,C,D,E,F,G,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val e = fresh[E]
    val f = fresh[F]
    val g = fresh[G]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d), wrap(e), wrap(f), wrap(g)).s }
    wrap{ stageEffectful(FunDecl7(a, b, c, d, e, f, g, blk), blk.effects)(ctx) }
  }
  @internal def decl8[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](func: Function8[A,B,C,D,E,F,G,H,R]): Func8[A,B,C,D,E,F,G,H,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val e = fresh[E]
    val f = fresh[F]
    val g = fresh[G]
    val h = fresh[H]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d), wrap(e), wrap(f), wrap(g), wrap(h)).s }
    wrap{ stageEffectful(FunDecl8(a, b, c, d, e, f, g, h, blk), blk.effects)(ctx) }
  }
  @internal def decl9[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](func: Function9[A,B,C,D,E,F,G,H,I,R]): Func9[A,B,C,D,E,F,G,H,I,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val e = fresh[E]
    val f = fresh[F]
    val g = fresh[G]
    val h = fresh[H]
    val i = fresh[I]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d), wrap(e), wrap(f), wrap(g), wrap(h), wrap(i)).s }
    wrap{ stageEffectful(FunDecl9(a, b, c, d, e, f, g, h, i, blk), blk.effects)(ctx) }
  }
  @internal def decl10[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](func: Function10[A,B,C,D,E,F,G,H,I,J,R]): Func10[A,B,C,D,E,F,G,H,I,J,R] = {
    val a = fresh[A]
    val b = fresh[B]
    val c = fresh[C]
    val d = fresh[D]
    val e = fresh[E]
    val f = fresh[F]
    val g = fresh[G]
    val h = fresh[H]
    val i = fresh[I]
    val j = fresh[J]
    val blk = stageBlock{ func(wrap(a), wrap(b), wrap(c), wrap(d), wrap(e), wrap(f), wrap(g), wrap(h), wrap(i), wrap(j)).s }
    wrap{ stageEffectful(FunDecl10(a, b, c, d, e, f, g, h, i, j, blk), blk.effects)(ctx) }
  }
}

object Func1 {
  implicit def func1IsStaged[A:Type,R:Type]: Type[Func1[A,R]] = Func1Type(typ[A],typ[R])
}
object Func2 {
  implicit def func2IsStaged[A:Type,B:Type,R:Type]: Type[Func2[A,B,R]] = Func2Type(typ[A],typ[B],typ[R])
}
object Func3 {
  implicit def func3IsStaged[A:Type,B:Type,C:Type,R:Type]: Type[Func3[A,B,C,R]] = Func3Type(typ[A],typ[B],typ[C],typ[R])
}
object Func4 {
  implicit def func4IsStaged[A:Type,B:Type,C:Type,D:Type,R:Type]: Type[Func4[A,B,C,D,R]] = Func4Type(typ[A],typ[B],typ[C],typ[D],typ[R])
}
object Func5 {
  implicit def func5IsStaged[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type]: Type[Func5[A,B,C,D,E,R]] = Func5Type(typ[A],typ[B],typ[C],typ[D],typ[E],typ[R])
}
object Func6 {
  implicit def func6IsStaged[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type]: Type[Func6[A,B,C,D,E,F,R]] = Func6Type(typ[A],typ[B],typ[C],typ[D],typ[E],typ[F],typ[R])
}
object Func7 {
  implicit def func7IsStaged[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type]: Type[Func7[A,B,C,D,E,F,G,R]] = Func7Type(typ[A],typ[B],typ[C],typ[D],typ[E],typ[F],typ[G],typ[R])
}
object Func8 {
  implicit def func8IsStaged[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type]: Type[Func8[A,B,C,D,E,F,G,H,R]] = Func8Type(typ[A],typ[B],typ[C],typ[D],typ[E],typ[F],typ[G],typ[H],typ[R])
}
object Func9 {
  implicit def func9IsStaged[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type]: Type[Func9[A,B,C,D,E,F,G,H,I,R]] = Func9Type(typ[A],typ[B],typ[C],typ[D],typ[E],typ[F],typ[G],typ[H],typ[I],typ[R])
}
object Func10 {
  implicit def func10IsStaged[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type]: Type[Func10[A,B,C,D,E,F,G,H,I,J,R]] = Func10Type(typ[A],typ[B],typ[C],typ[D],typ[E],typ[F],typ[G],typ[H],typ[I],typ[J],typ[R])
}

trait FunctionExp {
  @internal def fun[A:Type,R:Type](func: Function1[A,R]): Func1[A,R] = Func.decl1(func)
  @internal def fun[A:Type,B:Type,R:Type](func: Function2[A,B,R]): Func2[A,B,R] = Func.decl2(func)
  @internal def fun[A:Type,B:Type,C:Type,R:Type](func: Function3[A,B,C,R]): Func3[A,B,C,R] = Func.decl3(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,R:Type](func: Function4[A,B,C,D,R]): Func4[A,B,C,D,R] = Func.decl4(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](func: Function5[A,B,C,D,E,R]): Func5[A,B,C,D,E,R] = Func.decl5(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](func: Function6[A,B,C,D,E,F,R]): Func6[A,B,C,D,E,F,R] = Func.decl6(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](func: Function7[A,B,C,D,E,F,G,R]): Func7[A,B,C,D,E,F,G,R] = Func.decl7(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](func: Function8[A,B,C,D,E,F,G,H,R]): Func8[A,B,C,D,E,F,G,H,R] = Func.decl8(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](func: Function9[A,B,C,D,E,F,G,H,I,R]): Func9[A,B,C,D,E,F,G,H,I,R] = Func.decl9(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](func: Function10[A,B,C,D,E,F,G,H,I,J,R]): Func10[A,B,C,D,E,F,G,H,I,J,R] = Func.decl10(func)
}

/*@generate
trait FunctionCC {self: FunctionExp =>

  case class FunApplyJJ$JJ$1to22[TII$II$1toJJ, R:Type](fun: Exp[ArgonFunctionJJ[TII$II$1toJJ, R]], argII$II$1toJJ: Exp[TII])(implicit evII$II$1toJJ: Type[TII]
  ) extends Op[R] {
    def mirror(f: Tx): Exp[R] = {
      lazy val fargII$II$1toJJ = func(argII)
      fun_applyJJ(f(fun), fargII$II$1toJJ)
    }
  }

  case class FunDeclJJ$JJ$1to22[TII$II$1toJJ, R:Type](argII$II$1toJJ: Exp[TII], block: Block[R])(implicit evII$II$1toJJ: Type[TII]) extends Op[ArgonFunctionJJ[TII$II$1toJJ,R]] {
    def mirror(f: Tx) = stage(FunDeclJJ(argII$II$1toJJ, stageBlock{ val exp = f(block); exp() }))(ctx)
    override def binds = dyns(argII$II$1toJJ) ++ super.binds
  }

  case class ArgonFunctionJJ$JJ$1to22[TII$II$1toJJ, R:Type](s: Exp[ArgonFunctionJJ[TII$II$1toJJ, R]])(implicit evII$II$1toJJ: Type[TII]) extends MetaAny[ArgonFunctionJJ[TII$II$1toJJ,R]] with FunctionJJ+1[TII$II$1toJJ, State, R]  {
    @api def ===(that: ArgonFunctionJJ[TII$II$1toJJ, R]) = ???
    @api def =!=(that: ArgonFunctionJJ[TII$II$1toJJ, R]) = ???
    @api def toText = String.ify(this)

    @api def apply(xII$II$1toJJ: TII, st: State): R = applyArg(xII$II$1toJJ)
    @api def applyArg(xII$II$1toJJ: TII): R = wrap(fun_applyJJ(s, xII$II$1toJJ.s))
  }

  @api def argonFuncToFuncJJ$JJ$1to22(function: ArgonFunctionJJ)(implicit state: State) = (...) => fun(..., state)

}
trait FunctionExp extends FunctionCC {
  @generate  
  @internal def fun_applyJJ$JJ$1to22[TII$II$1toJJ, R:Type](f: Exp[ArgonFunctionJJ[TII$II$1toJJ,R]], argII$II$1toJJ: Exp[TII])(implicit evII$II$1toJJ: Type[TII]): Exp[R] = stage(FunApplyJJ(f, argII$II$1toJJ))(ctx)

  @generate  
  @internal def fun$JJ$1to22[TII$II$1toJJ, R:Type](f: FunctionJJ[TII$II$1toJJ, R])(implicit evII$II$1toJJ: Type[TII]): ArgonFunctionJJ[TII$II$1toJJ,R] = {
    lazy val argII$II$1toJJ = fresh[TII]
    lazy val wargII$II$1toJJ = wrap(argII)
    val bodyBlock = stageBlock(f(wargII$II$1toJJ).s)
    val sym = stage(FunDeclJJ(argII$II$1toJJ, bodyBlock))(ctx)
    wrap(sym)
  }

  /** Type classes **/
  @generate  
  case class ArgonFunctionJJType$JJ$1to22[TII$II$1toJJ, R](childTII$II$1toJJ: Type[TII], childR: Type[R]) extends Type[ArgonFunctionJJ[TII$II$1toJJ, R]] {
    override def wrapped(x: Exp[ArgonFunctionJJ[TII$II$1toJJ,R]]) = ArgonFunctionJJ(x)(childR, childTII$II$1toJJ)
    override def unwrapped(x: ArgonFunctionJJ[TII$II$1toJJ,R]) = x.s
    override def stagedClass = classOf[ArgonFunctionJJ[TII$II$1toJJ,R]]
    override def typeArguments = List(childTII$II$1toJJ, childR)
    override def isPrimitive: CBoolean = false
  }
  
  @generate
  implicit def argonFunctionJJ$JJ$1to22[TII$II$1toJJ, R:Type](implicit evII$II$1toJJ: Type[TII]): ArgonFunctionJJType[TII$II$1toJJ,R] = {
    ArgonFunctionJJType(evII$II$1toJJ, typ[R])
  }

  //implicit def liftFunctionJJ2ArgonFunction$JJ$1to22[TII$II$1toJJ, R:Type](implicit evII$II$1toJJ: Type[TII]) = new Lift[scala.FunctionJJ[TII$II$1toJJ, R], ArgonFunctionJJ[TII$II$1toJJ, R]] {
  //  override def apply(x: FunctionJJ[TII$II$1toJJ, R])(implicit ctx: SrcCtx) = fun(x)
  //}
}*/
