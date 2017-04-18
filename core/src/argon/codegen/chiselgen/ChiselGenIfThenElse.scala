package argon.codegen.chiselgen

import argon.core.Staging
import argon.ops.IfThenElseExp

trait ChiselGenIfThenElse extends ChiselCodegen {
  val IR: IfThenElseExp with Staging
  import IR._

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
