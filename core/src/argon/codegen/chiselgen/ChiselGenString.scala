package argon.codegen.chiselgen

import argon._
import argon.nodes._
import argon.utils.escapeString

trait ChiselGenString extends ChiselCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case StringType => "String"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[?]): String = c match {
    case Const(c: String) => escapeString(c)
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ToString(x) => emit(src"val $lhs = $x.toString")
    case StringConcat(x,y) => emit(src"val $lhs = $x + $y")
    case StringEquals(x,y) => emit(src"val $lhs = $x == $y")
    case StringDiffer(x,y) => emit(src"val $lhs = $x != $y")
    case _ => super.emitNode(lhs, rhs)
  }

}
