package argon.codegen.chiselgen

import argon.ops.FixPtExp

trait ChiselGenFixPt extends ChiselCodegen {
  val IR: FixPtExp
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

  override protected def hasFracBits(tp: Type[_]): Boolean = tp match {
      case IntType()  => false
      case LongType() => false
      case FixPtType(s,d,f) => if (f == 0) false else true
      case _ => super.hasFracBits(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (IntType(), Const(cc: BigDecimal)) => cc.toInt.toString + ".U(32.W)"
    case (LongType(), Const(cc: BigDecimal)) => cc.toLong.toString + ".L"
    case (FixPtType(s,d,f), Const(cc: BigDecimal)) => if (hasFracBits(c.tp)) s"Utils.FixedPoint($s,$d,$f,$cc)" else cc.toInt.toString + ".U(32.W)"
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
    case FixLt(x,y)  => emit(src"val $lhs = $x < $y")
    case FixLeq(x,y) => emit(src"val $lhs = $x <= $y")
    case FixNeq(x,y) => emit(src"val $lhs = $x =/= $y")
    case FixEql(x,y) => emit(src"val $lhs = $x === $y")
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
    case _ => super.emitNode(lhs, rhs)
  }
}
