package argon.codegen.scalagen

import argon.internals._
import argon.nodes._

trait ScalaGenIfThenElse extends ScalaCodegen {

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case IfThenElse(cond, thenp, elsep) =>
      open(src"val $lhs = {")
      open(src"if ($cond) { ")
      emitBlock(thenp)
      close("}")
      open("else {")
      emitBlock(elsep)
      close("}")
      close("}")

    case _ => super.emitNode(lhs, rhs)
  }

}
