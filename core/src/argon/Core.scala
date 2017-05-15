package argon

import argon.core.TestBenchFailed
import argon.lang.{Text, Void, Array => MetaArray}
import argon.traversal.CompilerPass
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

  protected def onException(t: Throwable): Unit = {
    IR.withLog(Config.cwd, Config.name + "_exception.log") {
      Config.verbosity = 10
      IR.log(t.getMessage)
      IR.log("")
      t.getStackTrace.foreach{elem => IR.log(elem.toString) }
    }
    IR.error(s"An exception was encountered while compiling ${Config.name}: ")
    IR.error(s"  ${t.getMessage}")
    IR.error(s"This is likely a compiler bug. A log file has been created at: ")
    IR.error(s"  ${Config.cwd}/${Config.name}_exception.log")
  }

  def main(sargs: Array[String]): Unit = {
    val defaultName = self.getClass.getName.replace("class ", "").replace('.','-').replace("$","") //.split('$').head
    System.setProperty("argon.name", defaultName)
    Config.name = defaultName
    Config.init()

    parseArguments(sargs.toSeq)

    IR.__stagingArgs = this.__stagingArgs
    Lib.__args = this.__stagingArgs

    try {
      IR.compileOrRun(main())
    }
    catch {case t: Throwable =>
      if (Config.verbosity > 0) {
        IR.report(t.getMessage)
        t.getStackTrace.foreach{elem => IR.report(elem.toString) }
      }
      else {
        onException(t)
      }
      sys.exit(-1)
    }
  }
}

trait LibCore {
  private[argon] var __args: scala.Array[java.lang.String] = _
  def stagingArgs = __args
  def args = __args
}

trait CompilerCore extends ArgonExp {
  val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]
  val testbench: Boolean = false

  lazy val state: State = new State
  lazy val args: MetaArray[Text] = MetaArray.input_arguments(EmptyContext, state)
  private[argon] var __stagingArgs: scala.Array[java.lang.String] = _
  def stagingArgs = __stagingArgs

  lazy val timingLog = createLog(Config.logDir, "9999 CompilerTiming.log")

  def checkErrors(start: Long, stageName: String): Unit = if (state.hadErrors) {
    val time = (System.currentTimeMillis - start).toFloat
    checkWarnings()
    error(s"""${state.errors} ${plural(state.errors,"error","errors")} found during $stageName""")
    error(s"Total time: " + "%.4f".format(time/1000) + " seconds")
    if (testbench) throw new TestBenchFailed(state.errors)
    else System.exit(state.errors)
  }
  def checkWarnings(): Unit = if (state.hadWarnings) {
    warn(s"""${state.warnings} ${plural(state.warnings, "warning","warnings")} found""")
  }

  def settings(): Unit = { }
  def createTraversalSchedule(): Unit = { }


  def compileOrRun(blk: => Unit): Unit = {
    // --- Setup
    state.reset() // Reset global state
    settings()
    createTraversalSchedule()

    if (Config.clearLogs) deleteExts(Config.logDir, ".log")
    report(c"Compiling ${Config.name} to ${Config.genDir}")
    if (Config.verbosity >= 2) report(c"Logging ${Config.name} to ${Config.logDir}")


    // --- Staging

    val start = System.currentTimeMillis()
    Globals.staging = true
    var block: Block[Void] = withLog(Config.logDir, "0000 Staging.log") { stageBlock { Void(blk).s } }
    Globals.staging = false

    if (state.graph.curEdgeId == 0) return  // Nothing was staged -- likely running in library mode (or empty program)

    // Exit now if errors were found during staging
    checkErrors(start, "staging")


    // --- Traversals

    for (t <- passes) {
      if (state.graph.VERBOSE_SCHEDULING) {
        state.graph.glog.close()
        state.graph.glog = createLog(Config.logDir + "/sched/", state.paddedPass + " " + t.name + ".log")
        withLog(state.graph.glog) {
          log(s"${state.pass} ${t.name}")
          log(s"===============================================")
        }
      }

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
        state.scopeCache.clear()
      }
    }
    if (state.graph.VERBOSE_SCHEDULING) state.graph.glog.close()


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
}



