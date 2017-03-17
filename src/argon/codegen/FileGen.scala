package argon.codegen

import java.io.PrintWriter

trait FileGen extends Codegen {
  import IR._

  protected def emitMain[S:BStaged](b: Block[S]): Unit

  override protected def process[S:BStaged](b: Block[S]): Block[S] = {

    val file = newStream("main")
    withStream(file) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }
  override protected def preprocess[S:BStaged](b: Block[S]) = {
    emitFileHeader()
    super.preprocess(b)
  }
  override protected def postprocess[S:BStaged](b: Block[S]) = {
    emitFileFooter()
    super.postprocess(b)
  }
}
