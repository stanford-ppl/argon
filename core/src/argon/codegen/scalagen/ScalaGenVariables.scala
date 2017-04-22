package argon.codegen.scalagen

import argon.core.Staging
import argon.ops.VariablesExp

trait ScalaGenVariables extends ScalaCodegen {
  val IR: Staging with VariablesExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case NewVar(init)    => emit(src"var $lhs = $init")
    case ReadVar(v)      => emit(src"val $lhs = $v")
    case AssignVar(v, x) => emit(src"val $lhs = { $v = $x }")
    case _ => super.emitNode(lhs, rhs)
  }
}
