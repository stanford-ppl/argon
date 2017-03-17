package argon.codegen.chiselgen

import argon.ops.FltPtExp

trait ChiselGenFltPt extends ChiselCodegen {
  val IR: FltPtExp
  import IR._

  override protected def remap(tp: BStaged[_]): String = tp match {
    case FloatType()  => "Float"
    case DoubleType() => "Double"
    case _ => super.remap(tp)
  }

  override protected def hasFracBits(tp: BStaged[_]): Boolean = tp match {
      case FloatType() => true
      case DoubleType() => true
      case _ => super.hasFracBits(tp)
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
      case FloatType()  => emit(src"val $lhs = chisel.util.Random.nextFloat()")
      case DoubleType() => emit(src"val $lhs = chisel.util.Random.nextDouble()")
    }
    case FltConvert(x) => lhs.tp match {
      case FloatType()  => emit(src"val $lhs = $x.toFloat")
      case DoubleType() => emit(src"val $lhs = $x.toDouble")
    }
    case _ => super.emitNode(lhs, rhs)
  }

}
