package argon.codegen.cppgen

import argon.core._
import argon.nodes._

trait CppGenFunc extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case FuncType(_) => throw new Exception("Remapping of function types is not yet supported!")
    case _ => super.remap(tp)
  }

  def paramify(e: Exp[_]): String = src"${e.tp} $e"

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FuncDecl(args, block) =>
      val params = args.map(paramify).mkString(",")
      val rt = remap(block.result.tp)
      val name = lhs.name.getOrElse(src"$lhs")
      withStream(getStream("functions", "cpp")) {
        open(src"$rt $name($params) {")
          emitBlock(block)
        close("}")
      }
      withStream(getStream("functions", "h")) {
        emit(src"$rt $name($params);")
      }

    case FuncCall(func, args) =>
      val name = func.name.getOrElse(src"$lhs")
      val params = args.map(quote).mkString(",")
      emit(src"${lhs.tp} $lhs = $name($params);")

    case _ => super.emitNode(lhs, rhs)
  }
}
