package argon.codegen.scalagen

import argon.ops.VoidExp

trait ScalaGenVoid extends ScalaCodegen {
  val IR: VoidExp
  import IR._

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(()) => "()"
    case _         => super.quoteConst(c)
  }
}
