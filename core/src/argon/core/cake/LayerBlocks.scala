package argon.core
package cake

import forge._
import scala.collection.mutable

trait LayerBlocks { self: ArgonCore =>
  /**
    * Computes an *external* summary for a sequence of nodes
    * (Ignores reads/writes on data allocated within the scope)
    */
  @stateful def summarizeScope(context: Seq[Sym[_]]): Effects = {
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
  @stateful private def stageScope[R](block: => Exp[R], temp: Freq, isolated: Boolean = false)(implicit state: State): (Exp[R], Effects, Seq[Sym[_]]) = {
    import state._

    val saveContext = context
    val saveCache = defCache
    context = Nil

    // In an isolated block, don't allow CSE with outside statements
    if (isolated) defCache = Map.empty

    val result = block
    val deps = context
    context = saveContext

    // Reset contents of defCache when staging cold blocks
    // Reasoning here is that statements can't move out of cold blocks, so it makes no sense to CSE across them
    if (temp == Freq.Cold) defCache = saveCache

    val effects = summarizeScope(deps)
    (result, effects, deps)
  }

  @stateful def stageBlock[R](block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false): Block[R] = {
    val (result, effects, effectful) = stageScope(block, temp)
    Block(Nil, result, effects, effectful, temp)
  }
  @stateful def stageLambda1[A,R](a: Exp[A])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false): Lambda1[A,R] = {
    val (result, effects, effectful) = stageScope(block, temp)
    Lambda1(a, result, effects, effectful, temp)
  }
  @stateful def stageLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false): Lambda2[A,B,R] = {
    val (result, effects, effectful) = stageScope(block, temp)
    Lambda2(a, b, result, effects, effectful, temp)
  }
  @stateful def stageLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false): Lambda3[A,B,C,R] = {
    val (result, effects, effectful) = stageScope(block, temp)
    Lambda3(a, b, c, result, effects, effectful, temp)
  }
  @stateful def stageLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false): Lambda4[A,B,C,D,R] = {
    val (result, effects, effectful) = stageScope(block, temp)
    Lambda4(a, b, c, d, result, effects, effectful, temp)
  }
  @stateful def stageLambdaN[R](inputs: Seq[Exp[_]], block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false): Block[R] = {
    val (result, effects, effectful) = stageScope(block, temp)
    Block(inputs, result, effects, effectful, temp)
  }

  @stateful def stageIsolatedBlock[T](block: => Exp[T]): Block[T] = stageBlock[T](block, temp = Freq.Cold, isolated = true)

  @stateful def stageColdBlock[R](block: => Exp[R]) = stageBlock(block, Freq.Cold)
  @stateful def stageColdLambda1[A,R](a: Exp[A])(block: => Exp[R]) = stageLambda1(a)(block, Freq.Cold)
  @stateful def stageColdLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]) = stageLambda2(a,b)(block, Freq.Cold)
  @stateful def stageColdLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R])= stageLambda3(a,b,c)(block, Freq.Cold)
  @stateful def stageColdLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R])= stageLambda4(a,b,c,d)(block, Freq.Cold)

  @stateful def stageHotBlock[R](block: => Exp[R]) = stageBlock(block, Freq.Hot)
  @stateful def stageHotLambda1[A,R](a: Exp[A])(block: => Exp[R]) = stageLambda1(a)(block, Freq.Hot)
  @stateful def stageHotLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]) = stageLambda2(a,b)(block, Freq.Hot)
  @stateful def stageHotLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R])= stageLambda3(a,b,c)(block, Freq.Hot)
  @stateful def stageHotLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R])= stageLambda4(a,b,c,d)(block, Freq.Hot)

  /**
    * Stage the effects of a block that is executed 'here' (if it is executed at all).
    * All assumptions about the current context carry over unchanged.
    * TODO: Not sure where this is useful yet, commenting out for now.
    */
  /*@stateful def stageBlockInline[T:Type](block: => Exp[T])(implicit state: State): Block[T] = {
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
  }*/
}
