package argon.core

import scala.collection.mutable

import forge._
import argon._

trait BlocksCore { self: ArgonCore =>
  /**
    * Computes an *external* summary for a sequence of nodes
    * (Ignores reads/writes on data allocated within the scope)
    */
  def summarizeScope(context: Seq[Sym[_]]): Effects = {
    var effects: Effects = Pure
    val allocs = new mutable.HashSet[Sym[_]]
    def clean(xs: Set[Sym[_]]) = xs diff allocs
    for (s@Effectful(u2, _) <- context) {
      if (u2.isMutable) allocs += s
      effects = effects andThen u2.copy(reads = clean(u2.reads), writes = clean(u2.writes))
    }
    effects
  }

  /**
    * Stage the effects of an isolated block.
    * No assumptions about the current context remain valid.
    */
  @stateful def createBlock[T:Type](block: => Exp[T], inputs: Seq[Sym[_]], temp: Freq)(implicit state: State): Block[T] = {
    import state._

    val saveContext = context
    val saveCache = defCache
    context = Nil

    val result = block
    val deps = context
    context = saveContext

    // Reset contents of defCache when staging cold blocks
    // Reasoning here is that statements can't move out of cold blocks, so it makes no sense to CSE across them
    if (temp == Freq.Cold) defCache = saveCache

    val effects = summarizeScope(deps)
    Block[T](result, effects, deps, inputs, temp)
  }

  @stateful def stageBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Normal)
  @stateful def stageLambda[T:Type](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, syms(inputs), Freq.Normal)

  @stateful def stageColdBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Cold)
  @stateful def stageColdLambda[T:Type](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, syms(inputs), Freq.Cold)

  @stateful def stageHotBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Hot)
  @stateful def stageHotLambda[T:Type](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, syms(inputs), Freq.Hot)


  /**
    * Stage the effects of a block that is executed 'here' (if it is executed at all).
    * All assumptions about the current context carry over unchanged.
    */
  @stateful def stageBlockInline[T:Type](block: => Exp[T])(implicit state: State): Block[T] = {
    import state._

    val saveContext = context
    if (saveContext eq null) state.context = Nil

    val result = block
    val nAdded = state.context.length - saveContext.length

    if ((saveContext ne null) && context.drop(nAdded) != saveContext)
      throw IllegalStageHereException(saveContext, context)

    val deps = if (saveContext eq null) context else context.take(nAdded) // Most recent effects

    val effects = summarizeScope(deps)
    context = saveContext

    Block[T](result, effects, deps, Nil, Freq.Normal)
  }
}
