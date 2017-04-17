package argon.codegen.pirgen

import argon.codegen.FileGen
import argon.Config

trait PIRFileGen extends FileGen {
  import IR._

  override protected def emitMain[S: Type](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def emitFileHeader() {

    emit("import pir.graph")
    emit("import pir.graph._")
    emit("import pir.graph.enums._")
    emit("import pir.codegen._")
    emit("import pir.plasticine.config._")
    emit("import pir.Design")
    emit("import pir.misc._")
    emit("import pir.PIRApp")
    emit("")
    open(s"""object ${Config.name}Design extends PIRApp {""")
    //emit(s"""override val arch = SN_4x4""")
    open(s"""def main(args: String*)(top:Top) = {""")

    super.emitFileHeader()
  }

  override protected def emitFileFooter() {
    emit(s"")
    close("}")
    close("}")

    super.emitFileFooter()
  }

}
