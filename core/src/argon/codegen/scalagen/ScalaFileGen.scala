package argon.codegen.scalagen

import argon.core._
import argon.codegen.FileGen

trait ScalaFileGen extends FileGen {
  override protected def emitMain[S:Type](b: Block[S]): Unit = {
    open(src"object Main {")
    open(src"def main(args: Array[String]): Unit = {")
    emitBlock(b)
    close(src"}")
    close(src"}")
  }
}
