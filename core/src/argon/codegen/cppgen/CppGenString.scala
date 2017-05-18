package argon.codegen.cppgen

import argon._
import argon.nodes._
import argon.utils.escapeString

trait CppGenString extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case StringType => "string"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: String) => escapeString(c)
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ToString(x) => emit(src"${lhs.tp} $lhs = std::to_string($x);")
    case StringConcat(x,y) => emit(src"${lhs.tp} $lhs = string_plus($x, $y);")
    case StringEquals(x,y) => emit(src"val $lhs = $x == $y")
    case StringDiffer(x,y) => emit(src"val $lhs = $x != $y")
    case _ => super.emitNode(lhs, rhs)
  }

}
