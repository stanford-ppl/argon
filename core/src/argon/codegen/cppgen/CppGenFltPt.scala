package argon.codegen.cppgen

import argon.core.Staging
import argon.ops.{FixPtExp, FltPtExp}

trait CppGenFltPt extends CppCodegen {
  val IR: FltPtExp with FixPtExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case FloatType()  => "double"
    case DoubleType() => "double"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (FloatType(), Const(c: BigDecimal)) => c.toString + "f"
    case (DoubleType(), Const(c: BigDecimal)) => c.toString
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FltNeg(x)   => emit(src"val $lhs = -$x")
    case FltAdd(x,y) => emit(src"val $lhs = $x + $y")
    case FltSub(x,y) => emit(src"val $lhs = $x - $y")
    case FltMul(x,y) => emit(src"val $lhs = $x * $y")
    case FltDiv(x,y) => emit(src"val $lhs = $x / $y")
    case FltLt(x,y)  => emit(src"val $lhs = $x < $y")
    case FltLeq(x,y) => emit(src"val $lhs = $x <= $y")
    case FltNeq(x,y) => emit(src"val $lhs = $x != $y")
    case FltEql(x,y) => emit(src"val $lhs = $x == $y")
    case FltRandom(x) => lhs.tp match {
      case FloatType()  => emit(src"val $lhs = cpp.util.Random.nextFloat()")
      case DoubleType() => emit(src"val $lhs = cpp.util.Random.nextDouble()")
    }
    case FltConvert(x) => lhs.tp match {
      case FloatType()  => emit(src"val $lhs = $x.toFloat")
      case DoubleType() => emit(src"val $lhs = $x.toDouble")
    }
    case FltPtToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"${lhs.tp} $lhs = $x;")
      case LongType() => emit(src"${lhs.tp} $lhs = $x;")
      case FixPtType(s,d,f) => emit(src"${lhs.tp} $lhs = $x;")
    }
    case StringToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"float $lhs = std::stof($x);")
      case FloatType()  => emit(src"float $lhs = std::stof($x);")
    }

    case _ => super.emitNode(lhs, rhs)
  }

}