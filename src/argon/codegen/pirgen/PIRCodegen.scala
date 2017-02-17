package argon.codegen.pirgen

import sys.process._
import scala.language.postfixOps
import argon.codegen.Codegen
import argon.Config
import argon.codegen.FileDependencies

trait PIRCodegen extends Codegen with FileDependencies { // FileDependencies extends Codegen already
  import IR._
  override val name = "PIR Codegen"
  override val lang: String = "pir"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    //emit(src"// results in ${b.result}")
  }

  final protected def emitController(b: Block[_]): Unit = {
    visitBlock(b)
    //emit(src"// results in ${b.result}")
  }

  override def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case lhs: Sym[_] => s"x${lhs.id}"
  }

}
