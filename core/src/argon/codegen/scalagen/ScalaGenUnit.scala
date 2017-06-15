package argon.codegen.scalagen

import argon.internals._
import argon.nodes._

trait ScalaGenUnit extends ScalaCodegen {
  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(()) => "()"
    case _ => super.quoteConst(c)
  }
}
