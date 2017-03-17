package argon.codegen.scalagen

import argon.codegen.FileGen

trait ScalaFileGen extends FileGen {
  import IR._

  override protected def emitMain[S:BStaged](b: Block[S]): Unit = {
    open(src"object Main {")
    open(src"def main(args: Array[String]): Unit = {")
    emitBlock(b)
    close(src"}")
    close(src"}")
  }
}
