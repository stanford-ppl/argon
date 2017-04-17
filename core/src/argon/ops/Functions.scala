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

  case class FunApplyJJ$JJ$1to22[TII$II$1toJJ:Type, R:Type](fun: Exp[ArgonFunctionJJ[TII$II$1toJJ, R]], argII$II$1toJJ: Exp[TII]) extends Op[R] {
    def mirror(f: Tx): Exp[R] = fun_applyJJ(f(fun), f$II$1toJJ(argII))
  }

  case class FunDeclJJ$JJ$1to22[TII$II$1toJJ:Type, R:Type](argII$II$1toJJ: Exp[TII], block: Block[R]) extends Op[ArgonFunctionJJ[TII$II$1toJJ,R]] {
    def mirror(f: Tx) = stage(FunDeclJJ(f$II$1toJJ(argII), stageBlock { f(block) }))(ctx)
    override def binds = dyns(argII$II$1toJJ) ++ super.binds
  }


  def fun_applyJJ$JJ$1to22[TII$II$1toJJ:Type, R:Type](f: Exp[ArgonFunctionJJ[TII$II$1toJJ,R]], arg: Exp[T1])(implicit ctx: SrcCtx): Exp[R] = stage(FunApplyJJ(f, arg))(ctx)

  case class ArgonFunctionJJ$JJ$1to22[TII$II$1toJJ:Type, R:Type](s: Exp[ArgonFunctionJJ[TII$II$1toJJ, R]]) extends MetaAny[ArgonFunctionJJ[TII$II$1toJJ,R]] with FunctionJJ[TII$II$1toJJ, R] {
    @api def ===(that: ArgonFunctionJJ[TII$II$1toJJ, R]) = ???
    @api def =!=(that: ArgonFunctionJJ[TII$II$1toJJ, R]) = ???
    @api def toText = textify(this)

    def apply(xII$II$1toJJ: TII): R = applyArg(xII$II$1toJJ)(EmptyContext)
    @api def applyArg(xII$II$1toJJ: TII): R = wrap(fun_applyJJ(s, xII$II$1toJJ.s))
  }

  def fun$JJ$1to22[TII$II$1toJJ:Type, R:Type](f: FunctionJJ[TII$II$1toJJ, R])(implicit ctx: SrcCtx): ArgonFunction1[TII$II$1toJJ,R] = {
    val argII$II$1toJJ = fresh[TII]
    val bodyBlock = stageBlock(f(wrap$II$1toJJ(argII)).s)
    val sym = stage(FunDeclJJ(argII$II$1toJJ, bodyBlock))(ctx)
    wrap(sym)
  }

  /** Type classes **/
  case class ArgonFunctionJJType$JJ$1to22[TII$II$1toJJ, R](childTII$II$1toJJ: Meta[TII], childR: Meta[R]) extends Meta[ArgonFunctionJJ[TII$II$1toJJ, R]] {
    override def wrapped(x: Exp[ArgonFunctionJJ[TII$II$1toJJ,R]]) = ArgonFunctionJJ(x)(childTII$II$1toJJ, childR)
    override def unwrapped(x: ArgonFunctionJJ[TII$II$1toJJ,R]) = x.s
    override def stagedClass = classOf[ArgonFunctionJJ[TII$II$1toJJ,R]]
    override def typeArguments = List(childTII$II$1toJJ, childR)
    override def isPrimitive: Boolean = false
  }

  implicit def argonFunctionJJ$JJ$1to22[TII$II$1toJJ:Meta, R:Meta]: ArgonFunctionJJType[TII$II$1toJJ,R] = ArgonFunctionJJType[TII$II$1toJJ,R](meta$II$1toJJ[TII], meta[R])



}
