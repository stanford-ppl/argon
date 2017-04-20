package argon.ops

import argon._
import forge._
import org.virtualized.EmptyContext

@generate
trait FunctionApi extends FunctionExp {
  self: ArgonApi =>

  type ArgonFunctionJJ$JJ$1to22[TII$II$1toJJ, R] <: FunctionJJ[TII$II$1toJJ, R]

}

@generate
trait FunctionExp {
  self: ArgonExp =>

  case class FunApplyJJ$JJ$1to22[TII$II$1toJJ, R:Type](fun: Exp[ArgonFunctionJJ[TII$II$1toJJ, R]], argII$II$1toJJ: Exp[TII])(implicit evII$II$1toJJ: Type[TII]) extends Op[R] {
    def mirror(f: Tx): Exp[R] = {
      lazy val fargII$II$1toJJ = f(argII)
      fun_applyJJ(f(fun), fargII$II$1toJJ)
    }
  }

  case class FunDeclJJ$JJ$1to22[TII$II$1toJJ, R:Type](argII$II$1toJJ: Exp[TII], block: Block[R])(implicit evII$II$1toJJ: Type[TII]) extends Op[ArgonFunctionJJ[TII$II$1toJJ,R]] {
    def mirror(f: Tx) = stage(FunDeclJJ(argII$II$1toJJ, stageBlock { f(block) }))(ctx)
    override def binds = dyns(argII$II$1toJJ) ++ super.binds
  }

  def fun_applyJJ$JJ$1to22[TII$II$1toJJ, R:Type](f: Exp[ArgonFunctionJJ[TII$II$1toJJ,R]], argII$II$1toJJ: Exp[TII])(implicit evII$II$1toJJ: Type[TII], ctx: SrcCtx): Exp[R] = stage(FunApplyJJ(f, argII$II$1toJJ))(ctx)

  case class ArgonFunctionJJ$JJ$1to22[TII$II$1toJJ, R:Type](s: Exp[ArgonFunctionJJ[TII$II$1toJJ, R]])(implicit evII$II$1toJJ: Type[TII]) extends MetaAny[ArgonFunctionJJ[TII$II$1toJJ,R]] with FunctionJJ[TII$II$1toJJ, R] {
    @api def ===(that: ArgonFunctionJJ[TII$II$1toJJ, R]) = ???
    @api def =!=(that: ArgonFunctionJJ[TII$II$1toJJ, R]) = ???
    @api def toText = textify(this)

    def apply(xII$II$1toJJ: TII): R = applyArg(xII$II$1toJJ)(EmptyContext)
    @api def applyArg(xII$II$1toJJ: TII): R = wrap(fun_applyJJ(s, xII$II$1toJJ.s))
  }

  def fun$JJ$1to22[TII$II$1toJJ, R:Type](f: FunctionJJ[TII$II$1toJJ, R])(implicit evII$II$1toJJ: Type[TII], ctx: SrcCtx): ArgonFunctionJJ[TII$II$1toJJ,R] = {
    lazy val argII$II$1toJJ = fresh[TII]
    lazy val wargII$II$1toJJ = wrap(argII)
    val bodyBlock = stageBlock(f(wargII$II$1toJJ).s)
    val sym = stage(FunDeclJJ(argII$II$1toJJ, bodyBlock))(ctx)
    wrap(sym)
  }

  /** Type classes **/
  case class ArgonFunctionJJType$JJ$1to22[TII$II$1toJJ, R](childTII$II$1toJJ: Type[TII], childR: Type[R]) extends Meta[ArgonFunctionJJ[TII$II$1toJJ, R]] {
    override def wrapped(x: Exp[ArgonFunctionJJ[TII$II$1toJJ,R]]) = ArgonFunctionJJ(x)(childR, childTII$II$1toJJ)
    override def unwrapped(x: ArgonFunctionJJ[TII$II$1toJJ,R]) = x.s
    override def stagedClass = classOf[ArgonFunctionJJ[TII$II$1toJJ,R]]
    override def typeArguments = List(childTII$II$1toJJ, childR)
    override def isPrimitive: Boolean = false
  }

  implicit def argonFunctionJJ$JJ$1to22[TII$II$1toJJ, R:Type](implicit evII$II$1toJJ: Type[TII]): ArgonFunctionJJType[TII$II$1toJJ,R] = {
    ArgonFunctionJJType(evII$II$1toJJ, meta[R])
  }

  //implicit def liftFunctionJJ2ArgonFunction$JJ$1to22[TII$II$1toJJ, R:Type](implicit evII$II$1toJJ: Type[TII]) = new Lift[scala.FunctionJJ[TII$II$1toJJ, R], ArgonFunctionJJ[TII$II$1toJJ, R]] {
  //  override def apply(x: FunctionJJ[TII$II$1toJJ, R])(implicit ctx: SrcCtx) = fun(x)
  //}


}
