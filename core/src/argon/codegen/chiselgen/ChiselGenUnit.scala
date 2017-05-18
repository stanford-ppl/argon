package argon.codegen.chiselgen

import argon._
import argon.nodes._

trait ChiselGenUnit extends ChiselCodegen {

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(()) => "()"
    case _ => super.quoteConst(c)
  }
}
