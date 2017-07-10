package argon.codegen.dotgen

import argon.core._
import argon.nodes._
import argon.util.escapeString

trait DotGenString extends DotCodegen {

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: String) => escapeString(c)
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ToString(x) => 
    case StringConcat(x,y) =>
    case StringEquals(x,y) =>
    case StringDiffer(x,y) =>
    case _ => super.emitNode(lhs, rhs)
  }

}
