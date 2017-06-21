package argon.core.cake

import forge._
import scala.collection.mutable

trait LayerBlocks { self: ArgonCake =>
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

  @stateful private def inSealed[T](x: => T): T = {
    val prevEffects = state.blockEffects
    state.blockEffects = prevEffects andAlso Sticky
    val result = x
    state.blockEffects = prevEffects
    result
  }

  /**
    * Stage the effects of an isolated block.
    * No assumptions about the current context remain valid.
    */
  @stateful private def stageScope[R](block: => Exp[R], temp: Freq, isolated: Boolean = false, seal: Boolean = false): (Exp[R], Effects, Seq[Sym[_]]) = {
    if (state == null) throw new argon.NullStateException

    val saveContext = state.context
    val saveCache = state.defCache
    state.context = Nil

    // In an isolated or sealed blocks, don't allow CSE with outside statements
    // CSE with outer scopes should only occur if symbols are not allowed to escape,
    // which isn't true in either of these cases
    if (isolated || seal || temp == Freq.Cold) state.defCache = Map.empty

    val result = if (seal) inSealed{ block } else block

    val deps = if (seal) state.context.collect{case sym@Effectful(eff,_) if eff != Pure => sym}
               else      state.context.collect{case sym@Effectful(eff,_) if eff != Sticky && eff != Pure => sym}

    state.context = saveContext

    // Reset contents of defCache
    // -- prevents CSEing across inescapable blocks
    if (isolated || seal || temp == Freq.Cold) state.defCache = saveCache

    val effects = summarizeScope(deps)
    (result, effects, deps)
  }

  @stateful def stageBlock[R](block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false, seal: Boolean = false): Block[R] = {
    val (result, effects, effectful) = stageScope(block, temp, isolated, seal)
    Block(Nil, result, effects, effectful, temp, isolated, seal)
  }
  @stateful def stageLambda1[A,R](a: Exp[A])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false, seal: Boolean = false): Lambda1[A,R] = {
    val (result, effects, effectful) = stageScope(block, temp, isolated, seal)
    Lambda1(a, result, effects, effectful, temp, isolated, seal)
  }
  @stateful def stageLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false, seal: Boolean = false): Lambda2[A,B,R] = {
    val (result, effects, effectful) = stageScope(block, temp, isolated, seal)
    Lambda2(a, b, result, effects, effectful, temp, isolated, seal)
  }
  @stateful def stageLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false, seal: Boolean = false): Lambda3[A,B,C,R] = {
    val (result, effects, effectful) = stageScope(block, temp, isolated, seal)
    Lambda3(a, b, c, result, effects, effectful, temp, isolated, seal)
  }
  @stateful def stageLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false, seal: Boolean = false): Lambda4[A,B,C,D,R] = {
    val (result, effects, effectful) = stageScope(block, temp, isolated, seal)
    Lambda4(a, b, c, d, result, effects, effectful, temp, isolated, seal)
  }
  @stateful def stageLambdaN[R](inputs: Seq[Exp[_]], block: => Exp[R], temp: Freq = Freq.Normal, isolated: Boolean = false, seal: Boolean = false): Block[R] = {
    val (result, effects, effectful) = stageScope(block, temp, isolated, seal)
    Block(inputs, result, effects, effectful, temp, isolated, seal)
  }

  @stateful def stageIsolatedBlock[T](block: => Exp[T], seal: Boolean = false): Block[T] = stageBlock[T](block, temp = Freq.Cold, isolated = true, seal)

  @stateful def stageColdBlock[R](block: => Exp[R]) = stageBlock(block, temp = Freq.Cold)
  @stateful def stageColdLambda1[A,R](a: Exp[A])(block: => Exp[R]) = stageLambda1(a)(block, temp = Freq.Cold)
  @stateful def stageColdLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]) = stageLambda2(a,b)(block, temp = Freq.Cold)
  @stateful def stageColdLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R])= stageLambda3(a,b,c)(block, temp = Freq.Cold)
  @stateful def stageColdLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R])= stageLambda4(a,b,c,d)(block, temp = Freq.Cold)

  @stateful def stageHotBlock[R](block: => Exp[R]) = stageBlock(block, temp = Freq.Hot)
  @stateful def stageHotLambda1[A,R](a: Exp[A])(block: => Exp[R]) = stageLambda1(a)(block, temp = Freq.Hot)
  @stateful def stageHotLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]) = stageLambda2(a,b)(block, temp = Freq.Hot)
  @stateful def stageHotLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R])= stageLambda3(a,b,c)(block, temp = Freq.Hot)
  @stateful def stageHotLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R])= stageLambda4(a,b,c,d)(block, temp = Freq.Hot)

  @stateful def stageSealedBlock[R](block: => Exp[R]): Block[R] = stageBlock[R](block, seal = true)
  @stateful def stageSealedLambda1[A,R](a: Exp[A])(block: => Exp[R]): Lambda1[A,R] = stageLambda1[A,R](a)(block, seal = true)
  @stateful def stageSealedLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: Exp[R]): Lambda2[A,B,R] = stageLambda2[A,B,R](a,b)(block, seal = true)

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
