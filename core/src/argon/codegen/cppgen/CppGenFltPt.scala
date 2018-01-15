package argon.codegen.cppgen

import argon.core._
import argon.nodes._
import argon.emul.FixedPoint

trait CppGenFltPt extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case FloatType()  => "float"
    case DoubleType() => "double"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (FloatType(), Const(c)) => c.toString
    case (DoubleType(), Const(c)) => c.toString
    case (FltPtType(m,e), Const(c)) => throw new Exception(s"Please avoid using unusual Float types ($m, $e).  Stick with (53,11) or (24,8).")
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FltNeg(x)   => emit(src"${lhs.tp} $lhs = -$x;")
    case FltAdd(x,y) => emit(src"${lhs.tp} $lhs = $x + $y;")
    case FltSub(x,y) => emit(src"${lhs.tp} $lhs = $x - $y;")
    case FltMul(x,y) => emit(src"${lhs.tp} $lhs = $x * $y;")
    case FltDiv(x,y) => emit(src"${lhs.tp} $lhs = $x / $y;")
    case FltLt(x,y)  => emit(src"${lhs.tp} $lhs = $x < $y;")
    case FltLeq(x,y) => emit(src"${lhs.tp} $lhs = $x <= $y;")
    case FltNeq(x,y) => emit(src"${lhs.tp} $lhs = $x != $y;")
    case FltEql(x,y) => emit(src"${lhs.tp} $lhs = $x == $y;")
    case FltRandom(x) => lhs.tp match {
      case FloatType()  => emit(src"${lhs.tp} $lhs = ((float) (rand() % ${x.getOrElse(100)})) / ${x.getOrElse(100)};")
      case DoubleType() => emit(src"${lhs.tp} $lhs = ((double) (rand() % ${x.getOrElse(100)})) / ${x.getOrElse(100)};")
    }
    case FltConvert(x) => lhs.tp match {
      case FloatType()  => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
      case DoubleType() => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
    }
    case FltPtToFixPt(x) => 
      lhs.tp match {
        case IntType()  => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
        case LongType() => emit(src"${lhs.tp} $lhs = (${lhs.tp}) $x;")
        case FixPtType(s,d,f) => emit(src"${lhs.tp} $lhs = $x;")
      }

    case StringToFltPt(x) => 
      lhs.tp match {
        case DoubleType() => emit(src"float $lhs = std::stof($x);")
        case FloatType()  => emit(src"float $lhs = std::stof($x);")
      }
      x match {
        case Def(ArrayApply(array, i)) => 
          array match {
            case Def(InputArguments()) => 
              val ii = i match {case c: Const[_] => c match {case Const(c: FixedPoint) => c.toInt; case _ => -1}; case _ => -1}
              if (cliArgs.contains(ii)) cliArgs += (ii -> s"${cliArgs(ii)} / ${lhs.name.getOrElse(s"${lhs.ctx}")}")
              else cliArgs += (ii -> lhs.name.getOrElse(s"${lhs.ctx}"))
            case _ =>
          }
        case _ =>          
      }


    case _ => super.emitNode(lhs, rhs)
  }

}
