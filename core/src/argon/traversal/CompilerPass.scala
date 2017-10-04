package argon.traversal

import argon.core._

/**
  * Common trait for all passes which can be run by the compiler,
  * including analysis, code generation, and transformers
  *
  * Extend this trait directly if you don't need to traverse the graph.
  * Otherwise, extend Traversal or IterativeTraversal
  */
trait CompilerPass { self =>
  var IR: State
  implicit def __state: State = IR

  def name: String = c"${self.getClass}"

  // --- Options
  var verbosity: Int = 0
  var shouldWarn: Boolean = true
  var needsInit: Boolean = true
  def shouldRun: Boolean = true
  def silence() { verbosity = -2; shouldWarn = false }
  def init(): Unit = {
    needsInit = false
    this.verbosity = IR.config.verbosity  // Can't do this in the constructor unfortunately :(
  }

  // --- State
  protected var tab = 0

  var lastTime  = 0.0f
  var totalTime = 0.0f

  // --- Debugging methods
  protected def dbgs(x: => Any) = dbg("  "*tab + x)
  protected def logs(x: => Any) = log("  "*tab + x)
  protected def msgs(x: => Any) = msg("  "*tab + x)

  // --- Methods
  /** External method called by compiler **/
  final def run[T:Type](b: Block[T]): Block[T] = if (shouldRun) {

    val outfile = state.paddedPass + " " + name + ".log"
    state.pass += 1

    def runTraversal() = {
      val start = System.currentTimeMillis

      val result = process(b)

      val time = (System.currentTimeMillis - start).toFloat
      lastTime = time
      totalTime += time

      result
    }

    if (this.verbosity >= 0) {
      withLog(config.logDir, outfile) {
        val saveVerbosity = config.verbosity
        val saveWarnings  = config.showWarn
        config.verbosity = this.verbosity
        config.showWarn  = this.shouldWarn

        log("Starting traversal " + name)
        val result = runTraversal()

        config.verbosity = saveVerbosity
        config.showWarn = saveWarnings

        result
      }
    }
    else {
      runTraversal()
    }

  } else b


  /** Called to execute this pass. Override to implement custom IR processing **/
  protected def process[S:Type](block: Block[S]): Block[S]
}
