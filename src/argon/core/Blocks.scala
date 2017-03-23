package argon.core

import argon.traversal.CompilerPass
import scala.collection.mutable

trait Blocks extends Effects { self: Staging =>
  type Pass = CompilerPass{ val IR: self.type }

  // --- State
  //private[argon] var eX: Option[CompilerPass] = None

  /** Class representing the result of a Staged scope. */
  case class Block[+T](result: Exp[T], summary: Effects, effectful: List[Sym[_]], inputs: Seq[Sym[_]]) {
    def tp: Staged[_] = result.tp
  }

  /**
    * Computes an *external* summary for a seq of nodes
    * (Ignores reads/writes on data allocated within the scope)
    */
  def summarizeScope(context: List[Sym[_]]): Effects = {
    var effects = Pure
    val allocs = new mutable.HashSet[Sym[_]]
    def clean(xs: Set[Sym[_]]) = xs diff allocs
    for (s@Effectful(u2, _) <- context) {
      if (u2.isMutable) allocs += s
      effects = effects andThen u2.copy(reads = clean(u2.reads), writes = clean(u2.writes))
    }
    effects
  }

  private def createBlock[T:Staged](block: => Exp[T], inputs: Seq[Sym[_]]): Block[T] = {
    val saveContext = context
    context = Nil

    val result = block
    val deps = context
    context = saveContext

    val effects = summarizeScope(deps)
    Block[T](result, effects, deps, inputs)
  }

  /**
    * Stage the effects of an isolated block.
    * No assumptions about the current context remain valid.
    */
  def stageBlock[T:Staged](block: => Exp[T]): Block[T] = createBlock[T](block, Nil)
  def stageLambda[T:Staged](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, onlySyms(inputs))

  /**
    * Stage the effects of a block that is executed 'here' (if it is executed at all).
    * All assumptions about the current context carry over unchanged.
    */
  def stageBlockInline[T: Staged](block: => Exp[T]): Block[T] = {
    val saveContext = context
    if (saveContext eq null) context = Nil

    val result = block
    val nAdded = context.length - saveContext.length

    if ((saveContext ne null) && context.drop(nAdded) != saveContext)
      throw IllegalStageHereException(saveContext, context)

    val deps = if (saveContext eq null) context else context.take(nAdded) // Most recent effects

    val effects = summarizeScope(deps)
    context = saveContext

    Block[T](result, effects, deps, Nil)
  }

  /** Compiler debugging **/
  override def readable(x: Any) = x match {
    case b: Block[_] if b.inputs.isEmpty => c"Block(${b.result})"
    case b: Block[_] => c"""Block(${b.inputs.mkString("(",",",")")} => ${b.result})"""
    case _ => super.readable(x)
  }
}
