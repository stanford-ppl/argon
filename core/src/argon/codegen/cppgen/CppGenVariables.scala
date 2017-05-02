package argon.codegen.cppgen

import argon.core.Staging
import argon.ops.{FixPtExp, FltPtExp, VariablesExp}

trait CppGenVariables extends CppCodegen {
  val IR: VariablesExp with Staging
  import IR._


  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case NewVar(init)    => emit(src"${lhs.tp} $lhs = $init;")
    case ReadVar(v)      => emit(src"${lhs.tp} $lhs = $v;")
    case AssignVar(v, x) => emit(src"$v = $x;")
    case _ => super.emitNode(lhs, rhs)

  }
}
