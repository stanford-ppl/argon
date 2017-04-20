package argon.codegen.scalagen

import argon.core.Staging
import argon.ops.FunctionExp
import forge.generate


@generate
trait ScalaGenFunction extends ScalaCodegen {
  val IR: FunctionExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case ArgonFunctionJJType$JJ$1to22(argII$II$1toJJ, r) =>
      val args = List(argII$II$1toJJ).map(remap).mkString(",")
      "(" + args + ") =>" + remap(r)
    case _ => super.remap(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FunDeclJJ$JJ$1to22(argII$II$1toJJ, block) =>
      val args = List(argII$II$1toJJ).map(x => quote(x) +": " + remap(x.tp)).mkString(",")
      val rt = remap(block.result.tp)
      val name = metadata[CtxName](lhs).get.name
      open(src"def $name($args): $rt = {")
      emitBlock(block)
      close("}")

    case FunApplyJJ$JJ$1to22(fun, argII$II$1toJJ) =>
      val name = metadata[CtxName](fun).get.name
      val args = List(argII$II$1toJJ).map(quote).mkString(",")
      emit(src"val $lhs: ${lhs.tp} = $name($args)")
    case _ => super.emitNode(lhs, rhs)
  }
}
