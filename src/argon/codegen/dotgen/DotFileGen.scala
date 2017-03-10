package argon.codegen.dotgen

import sys.process._
import scala.language.postfixOps
import argon.codegen.FileGen
import argon.Config

trait DotFileGen extends FileGen with DotCodegen {
  import IR._


  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
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
