package argon.codegen.dotgen

import argon.core.compiler._
import argon.nodes._

trait DotGenArray extends DotCodegen {

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => 
    case ArrayApply(array, i)   => 
    // case ArrayUpdate(array,i,e) => emit(src"val $lhs = $array.update($i, $e)")
    case ArrayLength(array)     => 
    case InputArguments()       => 
    case _ => super.emitNode(lhs, rhs)
  }
}
