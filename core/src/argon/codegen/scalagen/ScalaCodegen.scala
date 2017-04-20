package argon.codegen.scalagen

import argon.codegen.Codegen
import sys.process._
import scala.language.postfixOps
import argon.codegen.FileDependencies
import scala.io.Source
import java.io.File
import org.apache.commons.io._

trait ScalaCodegen extends Codegen with FileDependencies{

  import IR._
  override val name = "Scala Codegen"
  override val lang: String = "scala"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"${b.result}")
  }

}