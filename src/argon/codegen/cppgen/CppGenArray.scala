package argon.codegen.cppgen

import argon.ops.{ArrayExtExp, TextExp, FixPtExp, FltPtExp, BoolExp}

trait CppGenArray extends CppCodegen {
  val IR: ArrayExtExp with TextExp with FixPtExp with FltPtExp with BoolExp
  import IR._

  override protected def remap(tp: Staged[_]): String = tp match {
    case tp: ArrayType[_] => tp.typeArguments.head match {
        case DoubleType() => "cppDeliteArraydouble"
        case FloatType() => "cppDeliteArraydouble"
        case IntType() => "cppDeliteArrayint32_t"
        case LongType() => "cppDeliteArrayint32_t"
        case TextType => "cppDeliteArraystring"
        case BoolType => "cppDeliteArraybool"
        case _ => "genericArray of $tp"
      }
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    // Array constants are currently disallowed
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => emit(src"${lhs.tp}* $lhs = new ${lhs.tp}(*$size);")
    case ArrayApply(array, i)   => emit(src"${lhs.tp} $lhs = ${array}->apply($i);")
    case ArrayLength(array)     => emit(src"${lhs.tp} $lhs = $array.length;")
    case InputArguments()       => emit(src"${lhs.tp}* $lhs = args;")
    case _ => super.emitNode(lhs, rhs)
  }
}
