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
  def silence() { verbosity = -1 }

  // --- State
  protected var tab = 0

  final protected def debugs(x: => Any) = debug("  "*tab + x)
  final protected def msgs(x: => Any) = msg("  "*tab + x)

  final def traverse[T:Staged](b: Scope[T]): Scope[T] = if (shouldRun) {
    val outfile = State.paddedPass + " " + name + ".log"
    State.pass += 1
    if (verbosity >= 1) {
      withLog(Config.logDir, outfile){ runTraversal(b) }
    }
    else {
      withConsole{ runTraversal(b) }
    }
  } else b

  final protected def runTraversal[S:Staged](b: Scope[S]): Scope[S] = {
    val saveVerbosity = Config.verbosity
    Config.verbosity = this.verbosity

    msg("Starting traversal " + name)
    val start = System.currentTimeMillis

    val result = run(b)

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed traversal $name in " + "%.4f".format(time/1000) + " seconds")

    Config.verbosity = saveVerbosity
    result
  }

  final protected def runSingle[S:Staged](b: Scope[S]): Scope[S] = {
    val b2 = preprocess(b)
    val b3 = visitScope(b2)
    postprocess(b3)
  }

  /**
    * Called to execute this traversal, including optional pre- and post- processing.
    * Default is to run pre-processing, then a single traversal, then post-processing
    */
  protected def run[S:Staged](b: Scope[S]): Scope[S] = runSingle(b)
  protected def preprocess[S:Staged](b: Scope[S]): Scope[S] = { b }
  protected def postprocess[S:Staged](b: Scope[S]): Scope[S] = { b }
  protected def visitScope[S:Staged](b: Scope[S]): Scope[S] = {
    tab += 1
    traverseScope(b)
    tab -= 1
    b
  }


  override protected def visitStm(stm: Stm): Unit = {
    stm match {
      case TP(lhs, rhs)  => visit(lhs, rhs)
      case TTP(lhs, rhs) => visitFat(lhs, rhs)
    }
    if (recurse == Always)
      stm.rhs.scopes.foreach {blk => traverseScope(blk) }
  }

  protected def visit(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (recurse == Default)
      rhs.scopes.foreach {blk => traverseScope(blk) }
  }

  protected def visitFat(lhs: List[Sym[_]], rhs: Def): Unit = {}
}

trait IterativeTraversal extends Traversal {
  import IR._

  private var runs = 0                  // Current analysis iteration
  private var retries = 0               // Current retry
  private var _retry = false

  protected val MAX_ITERS: Int = 10     // maximum number of iterations to run
  protected val MAX_RETRIES: Int = 1    // maximum number of retries to allow
  protected def hasConverged: Boolean = runs > 0  // Condition for stopping
  protected def hasCompleted: Boolean = true      // Condition for being considered "complete"

  /**
    * Run traversal/analysis on a given block until convergence or maximum # of iterations reached
    *
    * Retry can be called to try to recover when visitor converged but did not complete
    * In postprocess, modify state, then call retry() to resume looping. Resets run number.
    * Can also implement auto-increase of MAX_ITERS using retry() in postprocess
    */
  protected def retry() { _retry = true }
  protected def failedToConverge() = throw new TraversalFailedToConvergeException(c"$name did not converge.")
  protected def failedToComplete() = throw new TraversalFailedToCompleteException(c"$name did not complete.")
  final protected def runIterative[S:Staged](b: Scope[S]): Scope[S] = {
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

    curBlock
  }


  override protected def run[S:Staged](b: Scope[S]): Scope[S] = runIterative(b)
}
