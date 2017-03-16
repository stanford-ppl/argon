package argon.codegen.cppgen

import argon.ops.{ArrayExtExp, TextExp, FixPtExp, FltPtExp, BoolExp, StructExp, TupleExp}

trait CppGenArray extends CppCodegen {
  val IR: ArrayExtExp with TextExp with FixPtExp with FltPtExp with BoolExp with StructExp with TupleExp
  import IR._


  protected def isArrayType(tp: Staged[_]): Boolean = tp match {
    case tp: ArrayType[_] => tp.typeArguments.head match {
      case tp: ArrayType[_] => println("EXCEPTION: Probably can't handle nested array types in ifthenelse"); true
      case _ => true
    }
    case _ => false
  }

  override protected def remap(tp: Staged[_]): String = tp match {
    case tp: ArrayType[_] => tp.typeArguments.head match {
        case DoubleType() => "vector<double>"
        case FloatType() => "vector<double>"
        case IntType() => "vector<int32_t>"
        case LongType() => "vector<int32_t>"
        case TextType => "cppDeliteArraystring"
        case BoolType => "vector<bool>"
        case fp: FixPtType[_,_,_] => 
          fp.fracBits match {
            case 0 => "vector<int32_t>"
            case _ => "vector<double>"
          }
        case _: FltPtType[_,_] => "double"
        // case struct: Tup2Type[_,_] => src"cppDeliteArray${tp.typeArguments.head}" // Let struct find appropriate name for this  
        case tp_inner: ArrayType[_] => s"vector<${remap(tp_inner)}>"
        case _ => src"genericArray of ${tp.typeArguments.head}"
      }
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    // Array constants are currently disallowed
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => 
      emit(src"${lhs.tp}* $lhs = new ${lhs.tp}($size);")
    case ArrayApply(array, i)   => 
      val asterisk = if (isArrayType(lhs.tp)) "*" else "" // TODO: Not sure why this is necessary
      val get = if (src"${array.tp}" == "cppDeliteArraystring") {
        emit(src"${lhs.tp}${asterisk} $lhs = ${array}->apply($i);")
      } else {
        emit(src"${lhs.tp}${asterisk} $lhs = (*${array})[$i];")
      }
    case ArrayLength(array)     => emit(src"${lhs.tp} $lhs = (*${array}).size();")
    case InputArguments()       => emit(src"${lhs.tp}* $lhs = args;")
    case _ => super.emitNode(lhs, rhs)
  }
}
