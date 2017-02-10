package argon.codegen.cppgen

import argon.ops.IfThenElseExp

trait CppGenIfThenElse extends CppCodegen {
  val IR: IfThenElseExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case IfThenElse(cond, thenp, elsep) =>
      emit(src"${lhs.tp} $lhs;")
      open(src"if ($cond) { ")
      emitBlock(thenp)
      emit(src"$lhs = ${thenp.result};")
      closeopen("} else { ")
      emitBlock(elsep)
      emit(src"$lhs = ${elsep.result};")
      close("}")

    case _ => super.emitNode(lhs, rhs)
  }

}
