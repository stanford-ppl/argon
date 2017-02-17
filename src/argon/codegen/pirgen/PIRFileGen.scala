package argon.codegen.pirgen

import argon.codegen.FileGen
import argon.Config
import sys.process._
import scala.language.postfixOps

trait PIRFileGen extends FileGen {
  import IR._


  //override protected def fileName:String = Config.name

  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
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
    open(s"""object ${Config.name} extends PIRApp {""")
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

  override protected def process[S:Staged](b: Block[S]): Block[S] = {
    super.process(b)
    //TODO: Cannot treat this as a dependency because postprocess is called before stream is closed
    // what should be the cleaner way of doing this?
    val sep = Config.sep
    val cmd = s"cp ${Config.genDir}${sep}pir${sep}main.scala ${sys.env("PIR_HOME")}${sep}apps${sep}src${sep}${Config.name}.scala" 
    println(cmd)
    cmd.!
    b
  }

}
