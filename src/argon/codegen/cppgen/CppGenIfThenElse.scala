package argon.codegen.cppgen

import argon.ops.IfThenElseExp
import argon.ops.{ArrayExtExp, TextExp, FixPtExp, FltPtExp, BoolExp}


trait CppGenIfThenElse extends CppGenArray {
  val IR: IfThenElseExp with ArrayExtExp with TextExp with FixPtExp with FltPtExp with BoolExp
  import IR._



  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case IfThenElse(cond, thenp, elsep) =>
      // Initialization of lhs is funky if we have array type
      if (isArrayType(lhs.tp)) {
        emit(src"// TODO: IfThenElse to set array type is tricky, only works for 1D now")
        emit(src"${lhs.tp}* $lhs;")
      } else { 
        emit(src"${lhs.tp} $lhs;")
      }
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
