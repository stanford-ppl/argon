package argon.traversal

import argon.core.Staging
import argon.{Config,State}

/**
  * Common trait for all passes which can be run by the compiler,
  * including analysis, code generation, and transformers
  *
  * Extend this trait directly if you don't need to traverse the graph.
  * Otherwise, extend Traversal or IterativeTraversal
  */
trait CompilerPass { self =>
  val IR: Staging
  import IR._

  def name: String = readable(self.getClass)

  // --- Options
  var verbosity: Int = Config.verbosity
  var shouldWarn: Boolean = true
  def shouldRun: Boolean = true
  def silence() { verbosity = -1; shouldWarn = false }

  // --- State
  protected var tab = 0

  // --- Debugging methods
  final protected def dbgs(x: => Any) = dbg("  "*tab + x)
  final protected def logs(x: => Any) = log("  "*tab + x)
  final protected def msgs(x: => Any) = msg("  "*tab + x)

  // --- Methods
  /** External method called by compiler **/
  final def run[T:Staged](b: Block[T]): Block[T] = if (shouldRun) {

    val outfile = State.paddedPass + " " + name + ".log"
    State.pass += 1

    withLog(Config.logDir, outfile) {
      val saveVerbosity = Config.verbosity
      val saveWarnings  = Config.showWarn
      Config.verbosity = this.verbosity
      Config.showWarn  = this.shouldWarn

      log("Starting traversal " + name)
      val start = System.currentTimeMillis

      val result = process(b)

      Config.verbosity = saveVerbosity
      Config.showWarn = saveWarnings

      val time = (System.currentTimeMillis - start).toFloat
      msg(s"  $name: " + "%.4f".format(time/1000))
      result
    }
  } else b


  /** Called to execute this pass. Override to implement custom IR processing **/
  protected def process[S:Staged](block: Block[S]): Block[S]
}
