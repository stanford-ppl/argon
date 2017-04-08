package argon
import argon.core.Staging
import argon.ops.ArrayExp
import argon.transform.Transformer
import argon.utils.deleteExts

import scala.collection.mutable.ArrayBuffer
import org.virtualized.{EmptyContext, SourceContext}

trait AppCore { self =>

  val IR: CompilerCore
  val Lib: LibCore

  private var __stagingArgs: scala.Array[java.lang.String] = _

  // Allows @virtualize def main(): Unit = { } and [@virtualize] def main() { }
  def main(): Unit

  def parseArguments(args: Seq[String]): Unit = {
    val parser = new ArgonArgParser
    parser.parse(args)
  }

  def main(sargs: Array[String]): Unit = {
    parseArguments(sargs.toSeq)
    IR.__stagingArgs = this.__stagingArgs
    Lib.__args = this.__stagingArgs

    Config.name = self.getClass.getName.replace("class ", "").replace('.','-').replace("$","") //.split('$').head
    Config.logDir =  Config.cwd + Config.sep + "logs" + Config.sep + Config.name
    IR.compileOrRun( main() )
  }
}

trait LibCore {
  private[argon] var __args: scala.Array[java.lang.String] = _
  def stagingArgs = __args
  def args = __args
}

trait CompilerCore extends Staging with ArrayExp { self =>
  val passes: ArrayBuffer[Pass] = ArrayBuffer.empty[Pass]
  val testbench: Boolean = false

  lazy val args: MetaArray[Text] = input_arguments()(EmptyContext)
  private[argon] var __stagingArgs: scala.Array[java.lang.String] = _
  def stagingArgs = __stagingArgs

  lazy val timingLog = createLog(Config.logDir, "9999 CompilerTiming.log")

  def settings(): Unit = { }

  def checkErrors(start: Long, stageName: String): Unit = if (hadErrors) {
    val time = (System.currentTimeMillis - start).toFloat
    checkWarnings()
    error(s"""$nErrors ${plural(nErrors,"error","errors")} found during $stageName""")
    error(s"Total time: " + "%.4f".format(time/1000) + " seconds")
    if (testbench) throw new TestBenchFailed(nErrors)
    else System.exit(nErrors)
  }
  def checkWarnings(): Unit = if (hadWarns) {
    warn(s"""$nWarns ${plural(nWarns, "warning","warnings")} found""")
  }


  def compileOrRun(blk: => Unit): Unit = {
    reset() // Reset global state
    settings()

    if (Config.clearLogs) deleteExts(Config.logDir, ".log")

    val start = System.currentTimeMillis()
    State.staging = true
    var block: Block[Void] = withLog(Config.logDir, "0000 Staging.log") { stageBlock { unit2void(blk).s } }
    State.staging = false

    if (curEdgeId == 0) return  // Nothing was Staged -- likely running in library mode (or empty program)

    // Exit now if errors were found during staging
    checkErrors(start, "staging")

    if (testbench) { Config.genDir = Config.cwd + Config.sep + "gen" + Config.sep + Config.name }
    report(c"Compiling ${Config.name} to ${Config.genDir}")

    for (t <- passes) {
      block = t.run(block)
      // After each traversal, check whether there were any reported errors
      checkErrors(start, t.name)

      if (Config.verbosity >= 1) withLog(timingLog) {
        msg(s"  ${t.name}: " + "%.4f".format(t.lastTime / 1000))
      }

      // Throw out scope cache after each transformer runs. This is because each block either
      // a. didn't exist before
      // b. existed but must be rescheduled now that it has new nodes
      if (t.isInstanceOf[Transformer]) {
        scopeCache.clear()
      }
    }

    val time = (System.currentTimeMillis - start).toFloat


    if (Config.verbosity >= 1) {
      withLog(timingLog) {
        msg(s"  Total: " + "%.4f".format(time / 1000))
        msg(s"")
        val totalTimes = passes.distinct.groupBy(_.name).mapValues{pass => pass.map(_.totalTime).sum }.toList.sortBy(_._2)
        for (t <- totalTimes) {
          msg(s"  ${t._1}: " + "%.4f".format(t._2 / 1000))
        }
      }
      timingLog.close()
    }


    checkWarnings()
    report(s"[\u001B[32mcompleted\u001B[0m] Total time: " + "%.4f".format(time/1000) + " seconds")
  }


  override def readable(x: Any): String = x match {
    case x: Tuple3[_,_,_] => c"${x._1} = ${x._2} [inputs = ${x._3}]"
    case _ => super.readable(x)
  }
}



