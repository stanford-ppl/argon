package argon.codegen.scalagen

import argon._
import argon.nodes._
import forge.generate

@generate
trait ScalaGenFunction extends ScalaCodegen {
  override protected def remap(tp: Type[_]): String = tp match {
    case FuncJJType$JJ$1to10(argII$II$1toJJ, r) =>
      val args = List(argII$II$1toJJ).map(remap).mkString(",")
      "(" + args + ") =>" + remap(r)
    case _ => super.remap(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FunDeclJJ$JJ$1to10(argII$II$1toJJ, block) =>
      val args = List(argII$II$1toJJ).map(x => quote(x) +": " + remap(x.tp)).mkString(",")
      val rt = remap(block.result.tp)
      val name = lhs.name.get
      open(src"def $name($args): $rt = {")
      emitBlock(block)
      close("}")

    case FunApplyJJ$JJ$1to10(fun, argII$II$1toJJ) =>
      val name = fun.name.get
      val args = List(argII$II$1toJJ).map(quote).mkString(",")
      emit(src"val $lhs: ${lhs.tp} = $name($args)")
    case _ => super.emitNode(lhs, rhs)
  }
}
