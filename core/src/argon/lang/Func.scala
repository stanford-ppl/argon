package argon.lang

import argon.core._
import argon.nodes._
import forge._

case class Func[R:Type](s: Exp[Func[R]]) extends MetaAny[Func[R]] {
  @api def ===(that: Func[R]): MBoolean = this.s == that.s
  @api def =!=(that: Func[R]): MBoolean = this.s != that.s
  @api def toText: MString = String.ify(this)
}

object Func {
  implicit def funcStagedIsStaged[R:Type]: Type[Func[R]] = FuncType(typ[R])

  @internal def call[R:Type](fun: Exp[Func[R]], l: List[Exp[_]]): Exp[R] = stage(FuncCall(fun, l))(ctx)

  @internal def decl[R:Type](l: List[Bound[_]], b: ()=>Exp[R]): Exp[Func[R]] = {
    val blk = stageSealedBlock{ b() }
    stageEffectful(FuncDecl(l, blk), blk.effects andAlso Funcd)(ctx)
  }
}
