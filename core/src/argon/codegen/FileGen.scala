package argon.codegen

import java.io.PrintWriter

trait FileGen extends Codegen {
  import IR._

  protected def emitMain[S:Type](b: Block[S]): Unit

  override protected def process[S:Type](b: Block[S]): Block[S] = {

    val file = newStream("main")
    withStream(file) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }
  override protected def preprocess[S:Type](b: Block[S]) = {
    emitFileHeader()
    super.preprocess(b)
  }
  override protected def postprocess[S:Type](b: Block[S]) = {
    emitFileFooter()
    super.postprocess(b)
  }
}
