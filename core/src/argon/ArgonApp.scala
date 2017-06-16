package argon

import argon.codegen.Codegen
import argon.core._
import compiler._
import argon.traversal.CompilerPass
import argon.transform.Transformer
import argon.util.deleteExts

import scala.collection.mutable.ArrayBuffer
import org.virtualized.SourceContext

trait ArgonApp { self =>
  /**
    * The entry function for users
    * Allows @virtualize def main(): Unit = { } and [@virtualize] def main() { }
    */
  def main(): Unit

  implicit var IR: State = _

  private var __stagingArgs: Array[String] = _
  private var __args: MArray[MString] = _
  def stagingArgs: Array[String] = __stagingArgs
  def args: MArray[MString] = __args

  final protected val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]
  final protected def codegenerators = passes.collect{case x: Codegen => x}

  protected val testbench: Boolean = false

  protected def checkErrors(state: State, start: Long, stageName: String): Unit = if (state.hadErrors) {
    val time = (System.currentTimeMillis - start).toFloat
    checkWarnings(state)
    error(s"""${state.errors} ${plural(state.errors,"error","errors")} found during $stageName""")
    error(s"Total time: " + "%.4f".format(time/1000) + " seconds")
    if (testbench) throw new TestBenchFailed(state.errors)
    else System.exit(state.errors)
  }
  protected def checkWarnings(state: State): Unit = if (state.hadWarnings) {
    warn(s"""${state.warnings} ${plural(state.warnings, "warning","warnings")} found""")
  }

  protected def parseArguments(args: Seq[String]): Unit = {
    val parser = new ArgonArgParser
    parser.parse(args)
  }

  protected def onException(t: Throwable): Unit = {
    withLog(core.Config.cwd, core.Config.name + "_exception.log") {
      core.Config.verbosity = 10
      log(t.getMessage)
      log("")
      t.getStackTrace.foreach{elem => log(elem.toString) }
    }
    error(s"An exception was encountered while compiling ${core.Config.name}: ")
    error(s"  ${t.getMessage}")
    error(s"This is likely a compiler bug. A log file has been created at: ")
    error(s"  ${core.Config.cwd}/${core.Config.name}_exception.log")
  }

  protected def settings(): Unit = { }
  protected def createTraversalSchedule(state: State): Unit = { }

  final protected def init(state: State): Unit = {
    state.reset() // Reset global state
    settings()
    createTraversalSchedule(state)

    __args = MArray.input_arguments()

    if (core.Config.clearLogs) deleteExts(core.Config.logDir, ".log")
    report(c"Compiling ${core.Config.name} to ${core.Config.genDir}")
    if (core.Config.verbosity >= 2) report(c"Logging ${core.Config.name} to ${core.Config.logDir}")
  }

  /**
    * Stage block
    */
  final protected def stageProgram[R:Type](blk: => R): Block[R] = {
    core.Globals.staging = true
    val block: Block[R] = withLog(core.Config.logDir, "0000 Staging.log") { stageBlock { blk.s } }
    core.Globals.staging = false
    block
  }

  final protected def runTraversals[R:Type](startTime: Long, b: Block[R], timingLog: Log): Unit = {
    var block: Block[R] = b
    // --- Traversals
    for (t <- passes) {
      if (state.graph.VERBOSE_SCHEDULING) {
        state.graph.glog.close()
        state.graph.glog = createLog(core.Config.logDir + "/sched/", state.paddedPass + " " + t.name + ".log")
        withLog(state.graph.glog) {
          log(s"${state.pass} ${t.name}")
          log(s"===============================================")
        }
      }

      block = t.run(block)
      // After each traversal, check whether there were any reported errors
      checkErrors(state, startTime, t.name)

      if (core.Config.verbosity >= 1) withLog(timingLog) {
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
  }


  protected def compileProgram(blk: => Unit): Unit = {
    // Spin up a new compiler state
    IR = new State
    init(state)

    val startTime = System.currentTimeMillis()

    val block = stageProgram{ MUnit(blk) }

    if (state.graph.curEdgeId == 0 && !testbench) {
      warn("Nothing staged, nothing gained.")
    }
    // Exit now if errors were found during staging
    checkErrors(state, startTime, "staging")

    val timingLog = createLog(core.Config.logDir, "9999 CompilerTiming.log")
    runTraversals(startTime, block, timingLog)

    val time = (System.currentTimeMillis - startTime).toFloat

    if (core.Config.verbosity >= 1) {
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

    checkWarnings(state)
    report(s"[\u001B[32mcompleted\u001B[0m] Total time: " + "%.4f".format(time/1000) + " seconds")
  }


  protected def initConfig(sargs: Array[String]): Unit = {
    val defaultName = self.getClass.getName.replace("class ", "").replace('.','-').replace("$","") //.split('$').head
    System.setProperty("argon.name", defaultName)
    core.Config.name = defaultName
    core.Config.init()

    parseArguments(sargs.toSeq)
  }

  /**
    * The "real" entry point for the application
    */
  def main(sargs: Array[String]): Unit = {
    __stagingArgs = sargs
    initConfig(sargs)

    try {
      compileProgram(main())
    }
    catch {case t: Throwable =>
      if (core.Config.verbosity > 0) {
        report(t.getMessage)
        t.getStackTrace.foreach{elem => report(elem.toString) }
      }
      else {
        onException(t)
      }
      if (!testbench) sys.exit(-1) else { throw t /* Rethrow exception during testbench compilation */ }
    }
  }
}
