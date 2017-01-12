package argon.codegen

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

trait SingleFileGen extends Codegen {
  import IR._

  protected def emitMain[S:Staged](b: Scope[S]): Unit

  override protected def run[S:Staged](b: Scope[S]): Scope[S] = {
    val path = s"$out${java.io.File.separator}$lang${java.io.File.separator}"
    Files.createDirectories(Paths.get(path))

    val file = new PrintWriter(s"${path}main.$ext")
    withStream(file) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }
  override protected def preprocess[S:Staged](b: Scope[S]) = {
    emitFileHeader()
    super.preprocess(b)
  }
  override protected def postprocess[S:Staged](b: Scope[S]) = {
    emitFileFooter()
    super.postprocess(b)
  }
}
