package argon.codegen.cppgen

import argon.core.Staging
import argon.ops.{FixPtExp, FltPtExp}

trait CppGenFixPt extends CppCodegen {
  val IR: FixPtExp with FltPtExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case IntType() => "int32_t"
    case LongType() => "int32_t"
    case FixPtType(s,d,f) => "double"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (IntType(), Const(c: BigDecimal)) => c.toInt.toString
    case (LongType(), Const(c: BigDecimal)) => c.toLong.toString + "L"
    case (FixPtType(s,d,f), Const(c: BigDecimal)) => c.toString
    case _ => super.quoteConst(c)
  }

  override protected def needsFPType(tp: Type[_]): Boolean = tp match {
      case IntType()  => false
      case LongType() => false
      case FixPtType(s,d,f) => if (f == 0) false else true
      case _ => super.needsFPType(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FixInv(x)   => emit(src"${lhs.tp} $lhs = ~$x;")
    case FixNeg(x)   => emit(src"${lhs.tp} $lhs = -$x;")
    case FixAdd(x,y) => emit(src"${lhs.tp} $lhs = $x + $y;")
    case FixSub(x,y) => emit(src"${lhs.tp} $lhs = $x - $y;")
    case FixMul(x,y) => emit(src"${lhs.tp} $lhs = $x * $y;")
    case FixDiv(x,y) => emit(src"${lhs.tp} $lhs = $x / $y;")
    case FixAnd(x,y) => emit(src"${lhs.tp} $lhs = $x & $y;")
    case FixOr(x,y)  => emit(src"${lhs.tp} $lhs = $x | $y;")
    case FixLt(x,y)  => emit(src"${lhs.tp} $lhs = $x < $y;")
    case FixLeq(x,y) => emit(src"${lhs.tp} $lhs = $x <= $y;")
    case FixNeq(x,y) => emit(src"${lhs.tp} $lhs = $x != $y;")
    case FixEql(x,y) => emit(src"${lhs.tp} $lhs = $x == $y;")
    case FixMod(x,y) => emit(src"${lhs.tp} $lhs = $x % $y;")
    case FixRandom(x) => lhs.tp match {
      case IntType()  => emit(src"${lhs.tp} $lhs = rand() % ${x.getOrElse(100)};")
      case LongType() => emit(src"${lhs.tp} $lhs = rand() % ${x.getOrElse(100)};")
      case _ => emit(src"${lhs.tp} $lhs = rand() % ${x.getOrElse(100)};")
    }
    case FixConvert(x) => lhs.tp match {
      case IntType()  => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
      case LongType() => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
      case FixPtType(s,d,f) => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;  // should be fixpt ${lhs.tp}")
    }
    case FixPtToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"${lhs.tp} $lhs = (int32_t) $x;")
      case FloatType()  => emit(src"${lhs.tp} $lhs = (int32_t) $x;")
    }
    case StringToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"int32_t $lhs = atoi(${x}.c_str());")
      case LongType() => emit(src"long $lhs = std::stol($x);")
      case FixPtType(s,d,f) => emit(src"float $lhs = std::stof($x);")
    }

    case _ => super.emitNode(lhs, rhs)
  }
}
