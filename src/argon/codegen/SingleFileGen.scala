package argon.codegen

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

trait SingleFileGen extends Codegen {
  import IR._

  protected def emitMain[S:Staged](b: Block[S]): Unit

  override protected def run[S:Staged](b: Block[S]): Block[S] = {
    Files.createDirectories(Paths.get(out))

    val file = new PrintWriter(s"${out}main.$ext")
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
