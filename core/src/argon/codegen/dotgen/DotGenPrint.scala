package argon.codegen.dotgen

import argon.core.compiler._
import argon.nodes._

trait DotGenPrint extends DotCodegen {

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case Print(x)   => 
    case Println(x) => 
    case _ => super.emitNode(lhs, rhs)
  }
}
