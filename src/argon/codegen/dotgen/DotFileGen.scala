package argon.codegen.dotgen

import argon.codegen.FileGen
import argon.Config

trait DotFileGen extends FileGen {
  import IR._


  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def emitFileHeader(): Unit = {
    open("digraph G {")
  }

  override protected def emitFileFooter(): Unit = {
    close("}")
  }

}
