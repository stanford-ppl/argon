package argon.codegen.cppgen

import argon.core.Staging
import argon.ops.FunctionExp
import forge.generate


@generate
trait CppGenFunction extends CppCodegen {
  val IR: FunctionExp with Staging
  import IR._


  override protected def remap(tp: Type[_]): String = tp match {
    case ArgonFunctionJJType$JJ$1to22(argII$II$1toJJ, r) =>
      println("Exception: c++ doesn't have first class functions so this should never be called")
      ???
    case _ => super.remap(tp)
  }

  def arg(e: Exp[_]): String = e.tp match {
    case ArgonFunctionJJType$JJ$1to22(argII$II$1toJJ, r) =>
      val args = List(argII$II$1toJJ).map(remap) .mkString(",")
      src"${remap(r)} (*$e)($args)"
    case a@_ => src"${remap(a)} $e"
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FunDeclJJ$JJ$1to22(argII$II$1toJJ, block) =>
      val args = List(argII$II$1toJJ).map(arg).mkString(",")
      val rt = remap(block.result.tp)
      withStream(getStream("functions", "cpp")) {
        open(src"$rt $lhs($args) = {")
        emitBlock(block)
        close("}")
      }
      withStream(getStream("functions", "h")) {
        emit(src"$rt $lhs($args);")
      }
    case FunApplyJJ$JJ$1to22(fun, argII$II$1toJJ) =>
      val args = List(argII$II$1toJJ).mkString(",")
      emit(src"val $lhs = $fun($args)")
    case _ => super.emitNode(lhs, rhs)
  }
}
