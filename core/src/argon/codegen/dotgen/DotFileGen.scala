package argon.codegen.dotgen

import argon.codegen.FileGen

trait DotFileGen extends FileGen with DotCodegen {
  import IR._

  override protected def emitMain[S:Type](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def emitFileHeader(): Unit = {
    open("digraph G {")
  }

  override protected def emitFileFooter(): Unit = {
    edges.foreach { edge => val e = edge() }
    close("}")
    val path = s"${out}main"
    //val cmd = s"dot -Tpdf ${path}.${ext} > ${path}.pdf" 
    //Console.println(cmd)
    //s"$cmd" !
  }

}
