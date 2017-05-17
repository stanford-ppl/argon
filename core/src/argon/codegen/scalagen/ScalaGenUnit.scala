package argon.codegen.scalagen

import argon._
import argon.nodes._

trait ScalaGenUnit extends ScalaCodegen {
  override protected def quoteConst(c: Const[?]): String = c match {
    case Const(()) => "()"
    case _ => super.quoteConst(c)
  }
}
