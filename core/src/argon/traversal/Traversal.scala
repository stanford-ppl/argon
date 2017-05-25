package argon.traversal

import argon.core.compiler._
import argon.core.TraversalFailedToCompleteException
import argon.core.TraversalFailedToConvergeException

/**
  * Single or iterative traversal of the IR with pre- and post- processing
  */
trait Traversal extends BlockTraversal with CompilerPass { self =>
  val IR: State
  override implicit val state: State = IR

  sealed abstract class RecurseOpt
  case object Always extends RecurseOpt
  case object Default extends RecurseOpt
  case object Never extends RecurseOpt

  // --- Options
  val recurse: RecurseOpt = Default   // Recursive traversal of IR hierarchy

  // --- Methods
  /** Run a single traversal, including pre- and post- processing **/
  final protected def runSingle[S:Type](b: Block[S]): Block[S] = {
    val b2 = preprocess(b)
    val b3 = visitBlock(b2)
    postprocess(b3)
  }

  /**
    * Called to execute this traversal, including optional pre- and post- processing.
    * Default is to run pre-processing, then a single traversal, then post-processing
    */
  protected def process[S:Type](block: Block[S]): Block[S] = runSingle(block)
  protected def preprocess[S:Type](block: Block[S]): Block[S] = { block }
  protected def postprocess[S:Type](block: Block[S]): Block[S] = { block }
  override protected def visitBlock[S](block: Block[S]): Block[S] = {
    tab += 1
    super.visitBlock(block)
    tab -= 1
    block
  }


  override protected def visitStm(stm: Stm): Unit = {
    stm match {
      case TP(lhs, rhs)  => visit(lhs, rhs)
      case TTP(lhs, rhs) => visitFat(lhs, rhs)
    }
    if (recurse == Always)
      stm.rhs.blocks.foreach {blk => visitBlock(blk) }
  }

  protected def visit(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (recurse == Default)
      rhs.blocks.foreach {blk => visitBlock(blk) }
  }

  protected def visitFat(lhs: Seq[Sym[_]], rhs: Def): Unit = {}
}


trait IterativeTraversal extends Traversal {

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
  final protected def runIterative[S:Type](b: Block[S]): Block[S] = {
    var curBlock = preprocess(b)
    do {
      runs = 0
      _retry = false
      while (!hasConverged && runs < MAX_ITERS) { // convergence condition
        runs += 1
        curBlock = visitBlock(curBlock)
      }
      curBlock = postprocess(curBlock)
      retries += 1
    } while (_retry && retries <= MAX_RETRIES)

    if (!hasCompleted) { failedToComplete() }
    else if (!hasConverged) { failedToConverge() }

    curBlock
  }


  override protected def process[S:Type](b: Block[S]): Block[S] = runIterative(b)
}
