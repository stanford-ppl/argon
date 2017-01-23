package argon.codegen.scalagen

import argon.codegen.SingleFileGen

trait ScalaSingleFileGen extends SingleFileGen {
  import IR._

  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    open(src"object Main {")
    open(src"def main(args: Array[String]): Unit = {")
    emitBlock(b)
    close(src"}")
    close(src"}")
  }
}
