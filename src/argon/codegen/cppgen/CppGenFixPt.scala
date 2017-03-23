package argon.codegen.cppgen

import argon.ops.FixPtExp

trait CppGenFixPt extends CppCodegen {
  val IR: FixPtExp
  import IR._

  override protected def remap(tp: Staged[_]): String = tp match {
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
    case _ => super.emitNode(lhs, rhs)
  }
}
