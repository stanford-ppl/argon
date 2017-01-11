package argon.traversal

import argon.core.Statements
import argon.{Config, State}

/**
  * Single or iterative traversal of the IR with pre- and post- processing
  */
trait Traversal extends ScopeTraversal { self =>
  val IR: Statements
  import IR._

  sealed abstract class RecurseOpt
  case object Always extends RecurseOpt
  case object Default extends RecurseOpt
  case object Never extends RecurseOpt

  // --- Options
  def name: String = readable(self.getClass)
  var verbosity: Int = Config.verbosity
  val recurse: RecurseOpt = Default   // Recursive traversal of IR hierarchy
  def shouldRun: Boolean = true

  protected val MAX_ITERS: Int = 10     // maximum number of iterations to run
  protected val MAX_RETRIES: Int = 1    // maximum number of retries to allow
  protected def hasConverged: Boolean = runs > 0  // Condition for stopping
  protected def hasCompleted: Boolean = true      // Condition for being considered "complete"

  // --- State
  private var runs = 0                  // Current analysis iteration
  private var retries = 0               // Current retry
  private var _retry = false
  protected var tab = 0

  def debugs(x: => Any) = debug("  "*tab + x)
  def msgs(x: => Any) = msg("  "*tab + x)

  final def pass[T:Staged](b: Scope[T]): Scope[T] = if (shouldRun) {
    val outfile = State.paddedPass + " " + name + ".log"
    State.pass += 1
    if (verbosity >= 1) {
      withLog(Config.logDir, outfile){ run(b) }
    }
    else {
      withConsole{ run(b) }
    }
  } else b

  def failedToConverge() = throw new TraversalFailedToConvergeException(c"$name did not converge.")
  def failedToComplete() = throw new TraversalFailedToCompleteException(c"$name did not complete.")

  /**
    * Function to be called to try to recover when visitor converged but did not complete
    * In postprocess, modify state, then call retry() to resume looping. Resets run number.
    * Can also implement auto-increase of MAX_ITERS using retry() in postprocess
    */
  def retry() { _retry = true }
  def silence() { verbosity = -1 }

  def preprocess[S:Staged](b: Scope[S]): Scope[S] = { b }
  def postprocess[S:Staged](b: Scope[S]): Scope[S] = { b }
  def visitScope[S:Staged](b: Scope[S]): Scope[S] = {
    tab += 1
    traverseScope(b)
    tab -= 1
    b
  }

  /**
    * Run traversal/analysis on a given block until convergence or maximum # of iterations reached
    */
  def run[S:Staged](b: Scope[S]): Scope[S] = if (shouldRun) {
    val saveVerbosity = Config.verbosity
    Config.verbosity = this.verbosity

    msg("Starting traversal " + name)
    val start = System.currentTimeMillis

    var curBlock = preprocess(b)
    do {
      runs = 0
      _retry = false
      while (!hasConverged && runs < MAX_ITERS) { // convergence condition
        runs += 1
        curBlock = visitScope(curBlock)
      }
      curBlock = postprocess(curBlock)
      retries += 1
    } while (_retry && retries <= MAX_RETRIES)

    if (!hasCompleted) { failedToComplete() }
    else if (!hasConverged) { failedToConverge() }

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed traversal $name in " + "%.4f".format(time/1000) + " seconds")

    Config.verbosity = saveVerbosity
    curBlock
  } else b

  override def visitStm(stm: Stm): Unit = {
    stm match {
      case Stm(List(lhs),rhs: Op[_]) => visit(lhs, rhs)
      case _ => visitFat(stm.lhs, stm.rhs)
    }
    if (recurse == Always) super.visitStm(stm)
  }

  def visit(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (recurse == Default) stmOf(lhs).foreach(super.visitStm)
  }

  def visitFat(lhs: List[Sym[_]], rhs: Def): Unit = {}
}
