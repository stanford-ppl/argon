package argon.codegen.scalagen

import argon._
import argon.nodes._

trait ScalaGenVariables extends ScalaCodegen {
  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case NewVar(init)    => emit(src"var $lhs = $init")
    case ReadVar(v)      => emit(src"val $lhs = $v")
    case AssignVar(v, x) => emit(src"val $lhs = { $v = $x }")
    case _ => super.emitNode(lhs, rhs)
  }
}
