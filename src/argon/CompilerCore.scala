package argon
import argon.core.Staging
import argon.ops.VoidExp
import argon.utils.deleteExts

import scala.collection.mutable.ArrayBuffer

trait CompilerCore extends Staging with VoidExp { self =>
  val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]

  def main(): Void
  def settings(): Unit = {}

  def main(args: Array[String]): Unit = {
    reset() // Reset global state
    settings()

    Config.name = c"${self.getClass}".replace('.','-')
    val start = System.currentTimeMillis()
    var block: Block[Void] = withLog(Config.logDir, "0000 Staging.log") { stageScope { main().s } }

    if (Config.clearLogs) deleteExts(Config.logDir, ".log")

    msg("--------------------------")
    msg(c"Compiling ${self.getClass}")
    for (t <- passes) {
      block = t.pass(block)
    }

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed in " + "%.4f".format(time/1000) + " seconds")
  }

  override def readable(x: Any): String = x match {
    case x: Tuple3[_,_,_] => c"${x._1} = ${x._2} [inputs = ${x._3}]"
    case _ => super.readable(x)
  }
}


