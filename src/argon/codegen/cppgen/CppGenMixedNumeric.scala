package argon.codegen.cppgen

import argon.ops.MixedNumericExp

trait CppGenMixedNumeric extends CppCodegen with CppGenFixPt with CppGenFltPt {
  val IR: MixedNumericExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case FixPtToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"val $lhs = $x.toDouble")
      case FloatType()  => emit(src"val $lhs = $x.toFloat")
    }
    case FltPtToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = $x.toInt")
      case LongType() => emit(src"val $lhs = $x.toLong")
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
