package argon.codegen.scalagen

import argon.core.Staging
import argon.ops.PrintExp

trait ScalaGenPrint extends ScalaCodegen {
  val IR: PrintExp with Staging
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case Print(x)   => emit(src"val $lhs = System.out.print($x)")
    case Println(x) => emit(src"val $lhs = System.out.println($x)")
    case _ => super.emitNode(lhs, rhs)
  }
}
