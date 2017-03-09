package argon.codegen.dotgen

import argon.ops.BoolExp

trait DotGenBool extends DotCodegen {
  val IR: BoolExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)       => 
    case And(x,y)     => 
    case Or(x,y)      => 
    case XOr(x,y)     => 
    case XNor(x,y)    => 
    case RandomBool(x) => 
    case _ => super.emitNode(lhs, rhs)
  }
}
