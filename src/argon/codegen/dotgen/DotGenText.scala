package argon.codegen.dotgen

import argon.ops.TextExp
import argon.utils.escapeString

trait DotGenText extends DotCodegen {
  val IR: TextExp
  import IR._

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: String) => escapeString(c)
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ToString(x) => 
    case TextConcat(x,y) => 
    case TextEquals(x,y) => 
    case TextDiffer(x,y) => 
    case _ => super.emitNode(lhs, rhs)
  }

}
