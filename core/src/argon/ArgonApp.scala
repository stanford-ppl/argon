package argon

import argon.codegen.Codegen
import argon.core._
import compiler._
import argon.traversal.CompilerPass
import argon.transform.Transformer
import argon.util.deleteExts

import scala.collection.mutable.ArrayBuffer
import org.virtualized.SourceContext

trait ArgonCompiler { self =>
  var _IR: State = new State
  final implicit def IR: State = _IR
  def name: String = self.getClass.getName.replace("class ", "").replace('.','_').replace("$","")

  def resetState(): Unit = {
    _IR = new State
    passes.clear()
  }

  def stagingArgs: Array[String]

  private var __args: MArray[MString] = _  
  def args: MArray[MString] = __args

  final protected val passes: ArrayBuffer[CompilerPass] = ArrayBuffer.empty[CompilerPass]
  final protected def codegenerators = passes.collect{case x: Codegen => x}

  protected val testbench: Boolean = false

  protected def checkBugs(start: Long, stageName: String): Unit = if (IR.hadBug) {
    onException(new Exception(s"Encountered compiler bug during pass $stageName"))
    if (testbench) throw new TestBenchFailed(1)
    else System.exit(1)
  }

  protected def checkErrors(start: Long, stageName: String): Unit = if (IR.hadErrors) {
    val time = (System.currentTimeMillis - start).toFloat
    checkWarnings()
    error(s"""${IR.errors} ${plural(IR.errors,"error","errors")} found during $stageName""")
    error(s"Total time: " + "%.4f".format(time/1000) + " seconds")
    if (testbench) throw new TestBenchFailed(IR.errors)
    else System.exit(IR.errors)
  }
  protected def checkWarnings(): Unit = if (IR.hadWarnings) {
    warn(s"""${IR.warnings} ${plural(IR.warnings, "warning","warnings")} found""")
  }

  protected def onException(t: Throwable): Unit = {
    withLog(config.logDir, config.name + "_exception.log") {
      config.verbosity = 10
      if (t.getMessage != null) { log(t.getMessage); log("") }
      if (t.getCause != null) { log(t.getCause); log("") }
      t.getStackTrace.foreach{elem => log(elem.toString) }
    }
    bug(s"An exception was encountered while compiling ${config.name}: ")
    if (t.getMessage != null) bug(s"  ${t.getMessage}")
    if (t.getCause != null) bug(s"  ${t.getCause}")
    bug(s"This is likely a compiler bug. A log file has been created at: ")
    bug(s"  ${config.logDir}/${config.name}_exception.log")
  }

  protected def settings(): Unit = { }
  protected def createTraversalSchedule(state: State): Unit = { }

  final protected def init(): Unit = {
    IR.reset() // Reset global state
    settings()
    createTraversalSchedule(IR)

    if (config.clearLogs) deleteExts(config.logDir, ".log")
    report(c"Compiling ${config.name} to ${config.genDir}")
    if (config.verbosity >= 2) report(c"Logging ${config.name} to ${config.logDir}")
  }

  /**
    * Stage block
    */
  final protected def stageProgram[R:Type](blk: => R): Block[R] = {
    core.Globals.staging = true
    val block: Block[R] = withLog(config.logDir, "0000 Staging.log") { stageBlock { blk.s } }
    core.Globals.staging = false
    block
  }

  final protected def runTraversals[R:Type](startTime: Long, b: Block[R], timingLog: Log): Unit = {
    var block: Block[R] = b

    if (IR.useBasicBlocks) warn("Using basic blocks!")

    // --- Traversals
    for (t <- passes) {
      if (IR.graph.VERBOSE_SCHEDULING) {
        IR.graph.glog.close()
        IR.graph.glog = createLog(config.logDir + "/sched/", IR.paddedPass + " " + t.name + ".log")
        withLog(IR.graph.glog) {
          log(s"${IR.pass} ${t.name}")
          log(s"===============================================")
        }
      }

      if (t.needsInit) t.init()

      block = t.run(block)
      // After each traversal, check whether there were any reported errors
      checkBugs(startTime, t.name)
      checkErrors(startTime, t.name)

      val v = config.verbosity
      config.verbosity = 1
      withLog(timingLog) {
        dbg(s"  ${t.name}: " + "%.4f".format(t.lastTime / 1000))
      }
      config.verbosity = v

      // Throw out scope cache after each transformer runs. This is because each block either
      // a. didn't exist before
      // b. existed but must be rescheduled now that it has new nodes
      if (t.isInstanceOf[Transformer]) {
        IR.scopeCache.clear()
      }
    }
    if (IR.graph.VERBOSE_SCHEDULING) IR.graph.glog.close()
  }


  protected def compileProgram(blk: () => Unit): Unit = {
    // Spin up a new compiler IR
    init()

    val startTime = System.currentTimeMillis()

    val block = stageProgram{
      __args = MArray.input_arguments()
      MUnit(blk())
    }

    if (IR.graph.curEdgeId == 0 && !testbench) {
      warn("Nothing staged, nothing gained.")
    }
    // Exit now if errors were found during staging
    checkBugs(startTime, "staging")
    checkErrors(startTime, "staging")

    val timingLog = createLog(config.logDir, "9999 CompilerTiming.log")
    runTraversals(startTime, block, timingLog)

    val time = (System.currentTimeMillis - startTime).toFloat

    val v = config.verbosity
    config.verbosity = 1
    withLog(timingLog) {
      dbg(s"  Total: " + "%.4f".format(time / 1000))
      dbg(s"")
      val totalTimes = passes.distinct.groupBy(_.name).mapValues{pass => pass.map(_.totalTime).sum }.toList.sortBy(_._2)
      for (t <- totalTimes) {
        dbg(s"  ${t._1}: " + "%.4f".format(t._2 / 1000))
      }
    }
    timingLog.close()
    config.verbosity = v

    checkWarnings()
    report(s"[\u001B[32mcompleted\u001B[0m] Total time: " + "%.4f".format(time/1000) + " seconds")
  }

  final def initConfig(sargs: Array[String]): Unit = {
    val config = createConfig()
    state.config = config
    System.setProperty("argon.name", name)
    config.name = name
    config.init()
  }

  protected def createConfig(): Config = new Config()
  protected def parseArguments(config: Config, sargs: Array[String]): Unit = {
    val parser = new ArgonArgParser(config)
    parser.parse(sargs.toSeq)
  }

}

trait ArgonApp extends ArgonCompiler { self =>

  protected var __stagingArgs: Array[String] = _
  def stagingArgs: Array[String] = __stagingArgs
  
  /**
    * The entry function for users
    * Allows @virtualize def main(): Unit = { } and [@virtualize] def main() { }
    */
  def main(): Unit


  /**
    * The "real" entry point for the application
    */
  def main(sargs: Array[String]): Unit = {
    __stagingArgs = sargs
    initConfig(sargs)

    try {
      compileProgram(() => main())
    }
    catch {
      case t: TestBenchFailed => throw t
      case t: Throwable =>
        onException(t)
        if (!testbench && config.verbosity < 1) sys.exit(-1) else throw t
    }
  }
}
