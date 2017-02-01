package argon.codegen.chiselgen

import argon.codegen.SingleFileGen

trait ChiselSingleFileGen extends SingleFileGen {
  import IR._

  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    open(src"object Main {")
    open(src"def main(args: Array[String]): Unit = {")
    emitBlock(b)
    close(src"}")
    close(src"}")
  }
}
