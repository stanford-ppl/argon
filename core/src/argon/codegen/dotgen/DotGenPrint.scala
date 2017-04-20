package argon.codegen.dotgen

import argon.core.Staging
import argon.ops.PrintExp

trait DotGenPrint extends DotCodegen {
  val IR: Staging with PrintExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case Print(x)   => 
    case Println(x) => 
    case _ => super.emitNode(lhs, rhs)
  }
}
