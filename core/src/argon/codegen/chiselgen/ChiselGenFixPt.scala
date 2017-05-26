package argon.codegen.chiselgen

import argon.core.compiler._
import argon.core.NoWireConstructorException
import argon.nodes._
import scala.math._

trait ChiselGenFixPt extends ChiselCodegen {

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

  protected def newWireFix(tp: Type[_]): String = tp match {
    case FixPtType(s,d,f) => src"new FixedPoint($s, $d, $f)"
    case IntType() => "UInt(32.W)"
    case LongType() => "UInt(32.W)"
    case _ => throw new NoWireConstructorException(s"$tp")
  }


  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (FixPtType(s,d,f), Const(cc: BigDecimal)) => 
      cc.toString + src".FP($s, $d, $f)"
    case (IntType(), Const(cc: BigDecimal)) => 
      if (cc >= 0) {
        cc.toString + ".toInt.U(32.W)"  
      } else {
        cc.toString + ".toInt.S(32.W).asUInt"
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
    case FixXor(x,y)  => emit(src"val $lhs = $x ^ $y")
    case FixLt(x,y)  => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x < $y")
    case FixLeq(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x <= $y")
    case FixNeq(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x =/= $y")
    case FixEql(x,y) => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x === $y")
    case FixMod(x,y) => emit(src"val $lhs = $x % $y")
    case UnbMul(x,y) => emit(src"val $lhs = $x *& $y")
    case UnbDiv(x,y) => emit(src"val $lhs = $x /& $y")
    case SatAdd(x,y) => emit(src"val $lhs = $x <+> $y")
    case SatSub(x,y) => emit(src"val $lhs = $x <-> $y")
    case SatMul(x,y) => emit(src"val $lhs = $x <*> $y")
    case SatDiv(x,y) => emit(src"val $lhs = $x </> $y")
    case FixLsh(x,y) => emit(s"val ${quote(lhs)} = ${quote(x)} << $y // TODO: cast to proper type (chisel expands bits)")
    case FixRsh(x,y) => emit(s"val ${quote(lhs)} = ${quote(x)} >> $y")
    case FixURsh(x,y) => emit(s"val ${quote(lhs)} = ${quote(x)} >>> $y")
    case UnbSatMul(x,y) => emit(src"val $lhs = $x <*&> $y")
    case UnbSatDiv(x,y) => emit(src"val $lhs = $x </&> $y")
    case FixRandom(x) => 
      val seed = (random*1000).toInt
      emit(s"val ${quote(lhs)}_bitsize = log2Up(${x.getOrElse(4096)}) max 1")
      emitGlobalModule(src"val ${lhs}_rng = Module(new PRNG($seed))")
      emitGlobalModule(src"${lhs}_rng.io.en := true.B")
      emit(src"val ${lhs} = ${lhs}_rng.io.output(${lhs}_bitsize,0)")
    case FixConvert(x) => lhs.tp match {
      case IntType()  => 
        emitGlobalWire(src"val $lhs = Wire(new FixedPoint(true, 32, 0))")
        emit(src"${lhs}.r := ${x}.r")
      case LongType() => 
        val pad = bitWidth(lhs.tp) - bitWidth(x.tp)
        emitGlobalWire(src"val $lhs = Wire(new FixedPoint(true, 64, 0))")
        if (pad > 0) {
          emit(src"${lhs}.r := chisel3.util.Cat(0.U(${pad}.W), ${x}.r)")
        } else {
          emit(src"${lhs}.r := ${x}.r.apply(${bitWidth(lhs.tp)-1}, 0)")
        }
      case FixPtType(s,d,f) => 
        emit(src"val $lhs = Wire(new FixedPoint($s, $d, $f))")
        emit(src"${x}.cast($lhs)")
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
