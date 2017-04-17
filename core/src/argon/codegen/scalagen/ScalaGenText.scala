package argon.codegen.scalagen

import argon.ops.TextExp
import argon.utils.escapeString

trait ScalaGenText extends ScalaCodegen {
  val IR: TextExp
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case TextType => "String"
    case _        => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: String) => escapeString(c)
    case _                => super.quoteConst(c)
  }

  def emitToString(lhs: Sym[_], x: Exp[_], tp: Type[_]) = tp match {
    case _ => emit(src"val $lhs = $x.toString")
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case e @ ToString(x)  => emitToString(lhs, x, e.mT)
    case TextConcat(x, y) => emit(src"val $lhs = $x + $y")
    case TextEquals(x, y) => emit(src"val $lhs = $x == $y")
    case TextDiffer(x, y) => emit(src"val $lhs = $x != $y")
    case _                => super.emitNode(lhs, rhs)
  }

}
