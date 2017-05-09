package argon.codegen.scalagen

import argon.core.Staging
import argon.ops.{FixPtExp, FltPtExp}

trait ScalaGenFixPt extends ScalaCodegen {
  val IR: FixPtExp with FltPtExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case IntType() => "Int"
    case LongType() => "Long"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (IntType(), Const(c: BigDecimal)) => c.toInt.toString
    case (LongType(), Const(c: BigDecimal)) => c.toLong.toString + "L"
    case _ => super.quoteConst(c)
  }

  /* WHY IS THIS FILE BEING IGNORED?! */
  
  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FixInv(x)   => emit(src"val $lhs = ~$x")
    case FixNeg(x)   => emit(src"val $lhs = -$x")
    case FixAdd(x,y) => emit(src"val $lhs = $x + $y")
    case FixSub(x,y) => emit(src"val $lhs = $x - $y")
    case FixMul(x,y) => emit(src"val $lhs = $x * $y")
    case FixDiv(x,y) => emit(src"val $lhs = $x / $y")
    case FixAnd(x,y) => emit(src"val $lhs = $x & $y")
    case FixOr(x,y)  => emit(src"val $lhs = $x | $y")
    case FixLt(x,y)  => emit(src"val $lhs = $x < $y")
    case FixLeq(x,y) => emit(src"val $lhs = $x <= $y")
    case FixNeq(x,y) => emit(src"val $lhs = $x != $y")
    case FixEql(x,y) => emit(src"val $lhs = $x == $y")
    case FixMod(x,y) => emit(src"val $lhs = $x % $y")
    case FixRandom(Some(max)) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = scala.util.Random.nextInt($max)")
      case LongType() => emit(src"val $lhs = scala.util.Random.nextLong() % $max")
    }
    case FixRandom(None) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = scala.util.Random.nextInt()")
      case LongType() => emit(src"val $lhs = scala.util.Random.nextLong()")
    }

    case FixConvert(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = $x.toInt")
      case LongType() => emit(src"val $lhs = $x.toLong")
    }
    case FixPtToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"val $lhs = $x.toDouble")
      case FloatType()  => emit(src"val $lhs = $x.toFloat")
    }
    case StringToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = $x.toInt")
      case LongType() => emit(src"val $lhs = $x.toLong")
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
