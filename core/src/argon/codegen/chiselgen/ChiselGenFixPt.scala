package argon.codegen.chiselgen

import argon.core.Staging
import argon.ops.{FixPtExp, FltPtExp}

trait ChiselGenFixPt extends ChiselCodegen {
  val IR: FixPtExp with FltPtExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case IntType() => "Int"
    case LongType() => "Long"
    case _ => super.remap(tp)
  }

  override protected def bitWidth(tp: Type[_]): Int = tp match {
      case IntType()  => 32
      case LongType() => 32 // or 64?
      case FixPtType(s,d,f) => d+f
      case _ => super.bitWidth(tp)
  }

  override protected def needsFPType(tp: Type[_]): Boolean = tp match {
      case FixPtType(s,d,f) => if (s) true else if (f == 0) false else true
      case IntType()  => false
      case LongType() => false
      case _ => super.needsFPType(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (FixPtType(s,d,f), Const(cc: BigDecimal)) => 
      if (s) {
        cc.toInt.toString + src".FP(true, $d, $f)"
      } else {
        cc.toInt.toString + ".U(32.W)"        
      }
    case (IntType(), Const(cc: BigDecimal)) => 
      if (cc >= 0) {
        cc.toInt.toString + ".U(32.W)"  
      } else {
        cc.toInt.toString + ".S(32.W).asUInt"
      }
      
    case (LongType(), Const(cc: BigDecimal)) => cc.toLong.toString + ".L"
    case (FixPtType(s,d,f), Const(cc: BigDecimal)) => 
      if (needsFPType(c.tp)) {s"Utils.FixedPoint($s,$d,$f,$cc)"} else {
        if (cc >= 0) cc.toInt.toString + ".U(32.W)" else cc.toInt.toString + ".S(32.W).asUInt"
      }
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FixInv(x)   => emit(src"val $lhs = ~$x")
    case FixNeg(x)   => emit(src"val $lhs = -$x")
    case FixAdd(x,y) => emit(src"val $lhs = $x + $y")
    case FixSub(x,y) => emit(src"val $lhs = $x - $y")
    case FixMul(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x * $y")
    case FixDiv(x,y) => emit(src"val $lhs = $x / $y")
    case FixAnd(x,y) => emit(src"val $lhs = $x & $y")
    case FixOr(x,y)  => emit(src"val $lhs = $x | $y")
    case FixLt(x,y)  => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x < $y")
    case FixLeq(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x <= $y")
    case FixNeq(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x =/= $y")
    case FixEql(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x === $y")
    case FixMod(x,y) => emit(src"val $lhs = $x % $y")
    case FixRandom(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = chisel.util.Random.nextInt()")
      case LongType() => emit(src"val $lhs = chisel.util.Random.nextLong()")
    }
    case FixConvert(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = $x // Fix to Fix")
      case LongType() => emit(src"val $lhs = $x // Fix to Long")
      case FixPtType(s,d,f) => emit(src"val $lhs = $x // should be fixpt ${lhs.tp}")
    }
    case FixPtToFltPt(x) => lhs.tp match {
      case DoubleType() => emit(src"val $lhs = $x.toDouble")
      case FloatType()  => emit(src"val $lhs = $x.toFloat")
    }
    case StringToFixPt(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = $x.toInt")
      case LongType() => emit(src"val $lhs = $x.toLong")
      case _ => emit(src"val $lhs = $x // No rule for this")
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
