package argon.codegen.cppgen

import argon.codegen.FileGen

trait CppFileGen extends FileGen {
  import IR._


  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def process[S:Staged](b: Block[S]): Block[S] = {

    // Forcefully create the following streams
    withStream(getStream("TopHost")) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }


  override protected def emitFileHeader() {
    super.emitFileHeader()
  }

  override protected def emitFileFooter() {  
    super.emitFileFooter()
  }

}
