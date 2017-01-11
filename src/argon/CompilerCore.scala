package argon
import argon.core.Staging
import argon.ops.ArrayExp
import argon.utils.deleteExts

import scala.collection.mutable.ArrayBuffer
import scala.virtualized.SourceContext

trait CompilerCore extends Staging with ArrayExp { self =>
  val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]
  val testbench: Boolean = false

  var args: MArray[Text] = _
  var stagingArgs: scala.Array[java.lang.String] = _

  def main(): scala.Unit
  def settings(): Unit = {}

  def checkErrors(start: Long, stageName: String): Unit = if (hadErrors) {
    val time = (System.currentTimeMillis - start).toFloat
    error(s"""$nErrors ${plural(nErrors,"error","errors")} found during $stageName""")
    error(s"Completed in " + "%.4f".format(time/1000) + " seconds")
    if (testbench) throw new TestBenchFailed(nErrors)
    System.exit(nErrors)
  }

  def main(sargs: Array[String]): Unit = {
    reset() // Reset global state
    settings()
    stagingArgs = sargs

    msg("--------------------------")
    msg(c"Staging ${self.getClass}")
    Config.name = c"${self.getClass}".replace('.','-')
    val start = System.currentTimeMillis()
    var block: Scope[Void] = withLog(Config.logDir, "0000 Staging.log") { stageBlock { unit2void(main()).s } }

    // Exit now if errors were found during staging
    checkErrors(start, "staging")

    msg("--------------------------")
    msg(c"Compiling ${self.getClass}")
    if (Config.clearLogs) deleteExts(Config.logDir, ".log")

    for (t <- passes) {
      block = t.pass(block)
      // After each traversal, check whether there were any reported errors
      checkErrors(start, t.name)
    }

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed in " + "%.4f".format(time/1000) + " seconds")
  }

  override def readable(x: Any): String = x match {
    case x: Tuple3[_,_,_] => c"${x._1} = ${x._2} [inputs = ${x._3}]"
    case _ => super.readable(x)
  }
}


