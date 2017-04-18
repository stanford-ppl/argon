package argon.codegen.chiselgen

import argon.core.Staging
import argon.ops.VoidExp

trait ChiselGenVoid extends ChiselCodegen {
  val IR: VoidExp with Staging
  import IR._

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(()) => "()"
    case _ => super.quoteConst(c)
  }
}
