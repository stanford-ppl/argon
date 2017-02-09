package argon.codegen.cppgen

import argon.ops.MixedNumericExp

trait CppGenMixedNumeric extends CppCodegen with CppGenFixPt with CppGenFltPt {
  val IR: MixedNumericExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case FixPtToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"${lhs.tp} $lhs = (int32_t) $x;")
      case FloatType()  => emit(src"${lhs.tp} $lhs = (int32_t) $x;")
    }
    case FltPtToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"${lhs.tp} $lhs = (int32_t) $x;")
      case LongType() => emit(src"${lhs.tp} $lhs = (int32_t) $x;")
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
