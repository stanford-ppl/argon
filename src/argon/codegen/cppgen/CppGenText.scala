package argon.codegen.cppgen

import argon.ops.TextExp
import argon.utils.escapeString

trait CppGenText extends CppCodegen {
  val IR: TextExp
  import IR._

  override protected def remap(tp: Staged[_]): String = tp match {
    case TextType => "String"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: String) => escapeString(c)
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ToString(x) => emit(src"string $lhs = convert_to_string< int32_t >($x);")
    case TextConcat(x,y) => emit(src"string $lhs = string_plus($x, $y);")
    case TextEquals(x,y) => emit(src"val $lhs = $x == $y")
    case TextDiffer(x,y) => emit(src"val $lhs = $x != $y")
    case _ => super.emitNode(lhs, rhs)
  }

}
