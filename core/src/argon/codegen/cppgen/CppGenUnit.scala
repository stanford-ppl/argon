package argon.codegen.cppgen

import argon._
import argon.nodes._

trait CppGenUnit extends CppCodegen {

  override protected def quoteConst(c: Const[?]): String = c match {
    case Const(()) => "()"
    case _ => super.quoteConst(c)
  }
}
