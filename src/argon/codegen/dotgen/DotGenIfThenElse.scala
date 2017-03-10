package argon.codegen.dotgen

import argon.ops.IfThenElseExp

trait DotGenIfThenElse extends DotCodegen {
  val IR: IfThenElseExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case IfThenElse(cond, thenp, elsep) =>

    case _ => super.emitNode(lhs, rhs)
  }

}
