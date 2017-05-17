package argon.codegen.scalagen

import argon._
import argon.codegen.{Codegen, FileDependencies}
import scala.language.postfixOps

trait ScalaCodegen extends Codegen with FileDependencies{
  override val name = "Scala Codegen"
  override val lang: String = "scala"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"${b.result}")
  }

}
