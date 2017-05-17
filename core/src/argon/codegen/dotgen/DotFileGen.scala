package argon.codegen.dotgen

import argon._
import argon.codegen.FileGen

trait DotFileGen extends FileGen with DotCodegen {

  override protected def emitMain[S:Type](b: Block[S]): Unit = {
    if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Begin!")}
    toggleEn() // Turn off
    emitBlock(b)
    toggleEn() // Turn on
    if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Complete!")}
  }

  override protected def emitFileHeader(): Unit = {
    open("digraph G {")
    emit(src"""
    title[label=<
        <FONT POINT-SIZE="40">Graph for: ${Config.name} (detail ${Config.dotDetail})</FONT>
    >,shape="box", color="blue",style="filled"];
""")
  }

  override protected def emitFileFooter(): Unit = {
    edges.foreach { edge => val e = edge() }
    close("}")
  }

}
