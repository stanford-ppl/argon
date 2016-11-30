package argon
import argon.graphs.HDAG
import argon.core.Core
import argon.ops.{VoidAPI, VoidCore}
import argon.utils.deleteExts

import scala.collection.mutable.ArrayBuffer


trait CompilerCore extends Core with VoidCore { self =>
  val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]

  def main(): Void
  def settings(): Unit = {}

  def main(args: Array[String]): Unit = {
    reset() // Reset global state
    settings()

    Config.name = c"${self.getClass}".replace('.','-')
    val start = System.currentTimeMillis()
    var block: Block[Void] = withLog(Config.logDir, "0000 Staging.log") { stageScope { main() } }

    if (Config.clearLogs) deleteExts(Config.logDir, ".log")

    msg("--------------------------")
    msg(c"Compiling ${self.getClass}")
    for (t <- passes) {
      block = t.pass(block)
    }

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed in " + "%.4f".format(time/1000) + " seconds")
  }

  private def appReadable(x: Any) = readable(x)

  val graph = new HDAG[Sym,Def,Metadata[_]] {
    override def readable(x: Any): String = x match {
      case x: Tuple3[_,_,_] => c"${x._1} = ${x._2} [inputs = ${x._3}]"
      case _ => appReadable(x)
    }
  }
}

trait AppCore extends VoidAPI {
  def main(): Void


}
