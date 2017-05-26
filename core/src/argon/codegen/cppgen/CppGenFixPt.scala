package argon.codegen.cppgen

import argon._
import argon.nodes._

trait CppGenFixPt extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case IntType() => "int32_t"
    case LongType() => "int32_t"
    case FixPtType(s,d,f) => 
      val u = if (!s) "u" else ""
      if (f > 0) {"double"} else {
        if (d > 32) s"${u}int64_t"
        else if (d > 16) s"${u}int32_t"
        else if (d > 8) s"${u}int16_t"
        else if (d > 4) s"${u}int8_t"
        else if (d > 2) s"${u}int8_t"
        else if (d == 2) s"${u}int8_t"
        else "bool"
      }
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
    case FixXor(x,y)  => emit(src"${lhs.tp} $lhs = $x ^ $y;")
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
      case DoubleType() => emit(src"${lhs.tp} $lhs = (double) $x;")
      case FloatType()  => emit(src"${lhs.tp} $lhs = (double) $x;")
    }
    case StringToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"int32_t $lhs = atoi(${x}.c_str());")
      case LongType() => emit(src"long $lhs = std::stol($x);")
      case FixPtType(s,d,f) => emit(src"float $lhs = std::stof($x);")
    }

    case _ => super.emitNode(lhs, rhs)
  }
}
