package argon.codegen.dotgen

import argon.ops.ArrayExp

trait DotGenArray extends DotCodegen {
  val IR: ArrayExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => 
    case ArrayApply(array, i)   => 
    // case ArrayUpdate(array,i,e) => emit(src"val $lhs = $array.update($i, $e)")
    case ArrayLength(array)     => 
    case InputArguments()       => 
    case _ => super.emitNode(lhs, rhs)
  }
}
