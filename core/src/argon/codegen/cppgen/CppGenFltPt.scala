package argon.codegen.cppgen

import argon._
import argon.nodes._

trait CppGenFltPt extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case FloatType()  => "double"
    case DoubleType() => "double"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[?]): String = (c.tp, c) match {
    case (FloatType(), Const(c: BigDecimal)) => c.toString
    case (DoubleType(), Const(c: BigDecimal)) => c.toString
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FltNeg(x)   => emit(src"${lhs.tp} $lhs = -$x;")
    case FltAdd(x,y) => emit(src"${lhs.tp} $lhs = $x + $y;")
    case FltSub(x,y) => emit(src"${lhs.tp} $lhs = $x - $y;")
    case FltMul(x,y) => emit(src"${lhs.tp} $lhs = $x * $y;")
    case FltDiv(x,y) => emit(src"${lhs.tp} $lhs = $x / $y;")
    case FltLt(x,y)  => emit(src"${lhs.tp} $lhs = $x < $y;")
    case FltLeq(x,y) => emit(src"${lhs.tp} $lhs = $x <= $y;")
    case FltNeq(x,y) => emit(src"${lhs.tp} $lhs = $x != $y;")
    case FltEql(x,y) => emit(src"${lhs.tp} $lhs = $x == $y;")
    case FltRandom(x) => lhs.tp match {
      case FloatType()  => emit(src"${lhs.tp} $lhs = cpp.util.Random.nextFloat();")
      case DoubleType() => emit(src"${lhs.tp} $lhs = cpp.util.Random.nextDouble();")
    }
    case FltConvert(x) => lhs.tp match {
      case FloatType()  => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
      case DoubleType() => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
    }
    case FltPtToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
      case LongType() => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
      case FixPtType(s,d,f) => emit(src"${lhs.tp} $lhs = $x;")
    }
    case StringToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"float $lhs = std::stof($x);")
      case FloatType()  => emit(src"float $lhs = std::stof($x);")
    }

    case _ => super.emitNode(lhs, rhs)
  }

}
