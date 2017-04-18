package argon.codegen.cppgen

import argon.core.Staging
import argon.ops.VoidExp

trait CppGenVoid extends CppCodegen {
  val IR: VoidExp with Staging
  import IR._

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(()) => "()"
    case _ => super.quoteConst(c)
  }
}
