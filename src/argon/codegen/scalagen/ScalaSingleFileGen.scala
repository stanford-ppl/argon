package argon.codegen.scalagen

import argon.codegen.SingleFileGen

trait ScalaSingleFileGen extends SingleFileGen {
  import IR._

  override protected def emitMain[S:Staged](b: Scope[S]): Unit = {
    open(src"object Main {")
    open(src"def main(args: Array[String]): Unit = {")
    emitScope(b)
    close(src"}")
    close(src"}")
  }
}
