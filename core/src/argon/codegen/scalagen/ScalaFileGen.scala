package argon.codegen.scalagen

import argon.core._
import argon.codegen.FileGen

trait ScalaFileGen extends FileGen {
  def emitPostMain(): Unit = {

  }

  override protected def emitMain[S:Type](b: Block[S]): Unit = {
    open(src"object Main {")
    open(src"def main(args: Array[String]): Unit = {")
    emitBlock(b)
    emitPostMain()
    close(src"}")
    open("def printHelp(): Unit = {")
    val argInts = cliArgs.toSeq.map(_._1)
    val argsList = if (argInts.length > 0) {
      (0 to argInts.max).map{i => 
        if (cliArgs.contains(i)) s"<$i- ${cliArgs(i)}>"
        else s"<${i}- UNUSED>"
      }.mkString(" ")
    } else {"<No input args>"}
    emit(s"""System.out.print("Help for app: ${config.name}\\n")""")
  	emit(s"""System.out.print("  -- bash run.sh ${argsList}\\n\\n");""")
  	emit(s"""System.exit(0);""")
    close("}")
    close(src"}")
  }
}
