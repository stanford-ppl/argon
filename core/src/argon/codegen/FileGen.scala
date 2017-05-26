package argon.codegen

import argon.core.compiler._
import org.apache.commons.io.FileUtils
import java.io.{File, IOException}

trait FileGen extends Codegen {

  protected def emitMain[S:Type](b: Block[S]): Unit

  protected def mainFile: String = "main"

  override protected def process[S:Type](b: Block[S]): Block[S] = {
    val file = newStream(mainFile)
    withStream(file) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }

  override protected def preprocess[S:Type](b: Block[S]) = {
    if (Config.clearGen) {
      try {
        FileUtils.deleteDirectory(new File(out))
      }
      catch { case _: IOException =>
        // Do nothing. It's fine if the folder didn't exist already
      }
    }
    emitFileHeader()
    super.preprocess(b)
  }

  override protected def postprocess[S:Type](b: Block[S]) = {
    emitFileFooter()
    super.postprocess(b)
  }
}
