package argon.codegen.scalagen

import argon.codegen.Codegen

trait ScalaCodegen extends Codegen {
  import IR._

  override val lang: String = "scala"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    traverseBlock(b)
    emit(src"${b.result}")
  }
  override protected def emitLambda(b: Lambda[_]): Unit = {
    traverseLambda(b)
    emit(src"${b.result}")
  }
  override protected def emitScope(b: Scope[_]): Unit = {
    traverseScope(b)
    emit(src"${b.result}")
  }
}
