package argon.codegen.chiselgen

import argon.ops.FixPtExp

trait ChiselGenFixPt extends ChiselCodegen {
  val IR: FixPtExp
  import IR._

  override protected def remap(tp: Staged[_]): String = tp match {
    case IntType() => "Int"
    case LongType() => "Long"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (IntType(), Const(c: BigInt)) => c.toInt.toString
    case (LongType(), Const(c: BigInt)) => c.toLong.toString + "L"
    case _ => super.quoteConst(c)
  }

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
    case FixRandom(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = chisel.util.Random.nextInt()")
      case LongType() => emit(src"val $lhs = chisel.util.Random.nextLong()")
    }
    case FixConvert(x) => lhs.tp match {
      case IntType()  => emit(src"val $lhs = $x.toInt")
      case LongType() => emit(src"val $lhs = $x.toLong")
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
