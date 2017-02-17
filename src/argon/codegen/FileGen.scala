package argon.codegen

import java.io.PrintWriter

trait FileGen extends Codegen {
  import IR._

  protected def emitMain[S:Staged](b: Block[S]): Unit

  protected def fileName:String = "main"
  override protected def process[S:Staged](b: Block[S]): Block[S] = {

    val file = newStream(fileName)
    withStream(file) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }

  override protected def preprocess[S:Staged](b: Block[S]) = {
    emitFileHeader()
    super.preprocess(b)
  }
  override protected def postprocess[S:Staged](b: Block[S]) = {
    emitFileFooter()
    super.postprocess(b)
  }
}
