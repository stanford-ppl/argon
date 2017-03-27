package argon.codegen.cppgen

import argon.ops.{ArrayExtExp, TextExp, FixPtExp, FltPtExp, BoolExp, IfThenElseExp, StructExp, TupleExp, HashMapExp}

trait CppGenArray extends CppCodegen {
  val IR: ArrayExtExp with TextExp with FixPtExp with FltPtExp with BoolExp with StructExp with TupleExp with HashMapExp with IfThenElseExp
  import IR._


  protected def isArrayType(tp: Type[_]): Boolean = tp match {
    case tp: ArrayType[_] => tp.typeArguments.head match {
      case tp: ArrayType[_] => println("EXCEPTION: Probably can't handle nested array types in ifthenelse"); true
      case _ => true
    }
    case _ => false
  }

  protected def getSize(array: Exp[_], extractor:String = ""): String = {
    src"(*${array})${extractor}.size()"
  }

  protected def emitNewArray(lhs: Exp[_], tp: Type[_], size: String): Unit = {
    emit(src"${tp}* $lhs = new ${tp}($size);")
  }

  protected def emitApply(dst: Exp[_], array: Exp[_], i: String, isDef: Boolean = true): Unit = {
    val get = if (src"${array.tp}" == "cppDeliteArraystring") {
      if (isDef) {
        emit(src"${dst.tp} $dst = ${array}->apply($i);")  
      } else {
        emit(src"$dst = ${array}->apply($i);")  
      }
    } else {
      if (isArrayType(dst.tp)) {
        val iterator = if ("^[0-9].*".r.findFirstIn(src"$i").isDefined) {src"${array}_applier"} else {src"$i"}
        if (isDef) {
          emit(src"""${dst.tp}* $dst = new ${dst.tp}(${getSize(array, src"[$i]")}); //cannot apply a vector from 2D vector, so make new vec and fill it, eventually copy the vector in the constructor here""")
          emit(src"for (int ${iterator}_sub = 0; ${iterator}_sub < (*${array})[${i}].size(); ${iterator}_sub++) { (*$dst)[${iterator}_sub] = (*${array})[$i][${iterator}_sub]; }")          
        } else {
          emit(src"for (int ${iterator}_sub = 0; ${iterator}_sub < (*${array})[${i}].size(); ${iterator}_sub++) { (*$dst)[${iterator}_sub] = (*${array})[$i][${iterator}_sub]; }")          
        }
      } else {
        if (isDef) {
          emit(src"${dst.tp} $dst = (*${array})[$i];")  
        } else {
          emit(src"$dst = (*${array})[$i];")
        }
      }
    }
  }

  override protected def remap(tp: Type[_]): String = tp match {
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
        case st: StructType[_] => src"vector<${tp.typeArguments.head}>"
        case _ => src"genericArray of ${tp.typeArguments.head}"
      }
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    // Array constants are currently disallowed
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => emitNewArray(lhs, lhs.tp, src"$size")
    case ArrayApply(array, i)   => emitApply(lhs, array, src"$i")
    case ArrayLength(array)     => emit(src"${lhs.tp} $lhs = ${getSize(array)};")
    case InputArguments()       => emit(src"${lhs.tp}* $lhs = args;")
    case _ => super.emitNode(lhs, rhs)
  }
}
