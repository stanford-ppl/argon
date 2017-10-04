package argon.codegen.dotgen

import argon.core._
import argon.codegen.FileGen

trait DotFileGen extends FileGen with DotCodegen {

  override protected def emitMain[S:Type](b: Block[S]): Unit = {
    if (config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Begin!")}
    toggleEn() // Turn off
    emitBlock(b)
    toggleEn() // Turn on
    if (config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Complete!")}
  }

  override protected def emitFileHeader(): Unit = {
    open("digraph G {")
    emit(src"""
    title[label=<
        <FONT POINT-SIZE="40">Graph for: ${config.name} (detail ${config.dotDetail})</FONT>
    >,shape="box", color="blue",style="filled"];
""")
  }

  override protected def emitFileFooter(): Unit = {
    edges.foreach { edge => val e = edge() }
    close("}")
  }

}
