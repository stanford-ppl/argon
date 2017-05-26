package argon.codegen.dotgen

import argon.core.compiler._
import argon.nodes._

trait DotGenIfThenElse extends DotCodegen {

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case IfThenElse(cond, thenp, elsep) =>

    case _ => super.emitNode(lhs, rhs)
  }

}
