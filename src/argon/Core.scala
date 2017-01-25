package argon
import argon.core.Staging
import argon.ops.{ArrayApi, ArrayExp}
import argon.utils.deleteExts

import scala.collection.mutable.ArrayBuffer
import org.virtualized.SourceContext

trait AppCore { self =>
  val IR: CompilerCore
  val Lib: LibCore // Should include "def args: Array[String] = self.stagingArgs"

  private var __stagingArgs: scala.Array[java.lang.String] = _
  def stagingArgs: Array[String] = __stagingArgs

  def main(): scala.Unit

  final def main(sargs: scala.Array[java.lang.String]): Unit = {
    __stagingArgs = sargs
    Config.name = self.getClass.getName.split('$').last.replace("class ", "").replace('.','-')
    IR.compileOrRun( main() )
  }
}

trait LibCore {
  // Nothing here for now
}

trait CompilerCore extends Staging with ArrayExp { self =>
  val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]
  val testbench: Boolean = false

  lazy val args: MArray[Text] = StagedArray[Text](stage(InputArguments())(implicitly[SourceContext]))
  var stagingArgs: scala.Array[java.lang.String] = _

  def settings(): Unit = { }

  def checkErrors(start: Long, stageName: String): Unit = if (hadErrors) {
    val time = (System.currentTimeMillis - start).toFloat
    error(s"""$nErrors ${plural(nErrors,"error","errors")} found during $stageName""")
    error(s"Completed in " + "%.4f".format(time/1000) + " seconds")
    if (testbench) throw new TestBenchFailed(nErrors)
    else System.exit(nErrors)
  }

  def compileOrRun(blk: => Unit): Unit = {
    reset() // Reset global state
    settings()

    msg("--------------------------")
    msg(c"Staging ${self.getClass}")

    val start = System.currentTimeMillis()
    var block: Block[Void] = withLog(Config.logDir, "0000 Staging.log") { stageBlock { unit2void(blk).s } }

    if (curEdgeId == 0) return  // Nothing was staged -- likely running in library mode (or empty program)

    // Exit now if errors were found during staging
    checkErrors(start, "staging")

    msg("--------------------------")
    msg(c"Compiling ${self.getClass}")
    if (Config.clearLogs) deleteExts(Config.logDir, ".log")

    for (t <- passes) {
      block = t.traverse(block)
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



