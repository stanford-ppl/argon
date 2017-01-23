package argon.codegen.scalagen

import argon.codegen.Codegen

trait ScalaCodegen extends Codegen {
  import IR._
  override val name = "Scala Codegen"
  override val lang: String = "scala"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"${b.result}")
  }
}
