package argon.core.cake

import argon.core.BlockProperties
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
    *
    * isolated: disallow CSE from outside of block, override outer 'sealed' block (inner statements aren't sticky)
    * seal:     disallow CSE from outside of block, add 'sticky' effects to all staged symbols
    * prog:     keep all 'Funcd' (function definition) effects as anti-dependencies
    * temp:     frequency hints to code motion - Cold also disallows CSE from outside block
    */
  @stateful private def stageScope[R](block: => Exp[R], properties: BlockProperties): (Exp[R], Effects, Seq[Sym[_]], List[Stm]) = {
    val isolated = properties.isolated
    val seal = properties.seal
    val temp = properties.temp
    val prog = properties.prog
    if (state == null) throw new argon.NullStateException

    val saveContext = state.context
    val saveCache = state.defCache
    val saveBlock = state.currentBlock
    val saveEffects = state.blockEffects
    state.context = Nil
    state.currentBlock = Nil
    // In an isolated or sealed blocks, don't allow CSE with outside statements
    // CSE with outer scopes should only occur if symbols are not allowed to escape,
    // which isn't true in either of these cases
    if (isolated || seal || temp == Freq.Cold) state.defCache = Map.empty
    if (isolated) state.blockEffects = Pure

    val result = if (seal) inSealed{ block } else block

    def keepEffect(e: Effects): Boolean = {
      if (e.isFunction) prog          // Only keep function definitions at the program level
      else if (seal) e != Pure        // Keep everything except pure effects
      else e != Sticky && e != Pure   // Keep everything except pure and sticky effects
    }

    log("")
    log(c"Completing staging of block. Context: ")
    state.context.foreach{sym => log(s"  ${str(sym)}") }

    val (deps,remain) = state.context.partition{
      case sym@Effectful(e,_) =>
        val keep = keepEffect(e)
        log(c"  ${str(sym)} [effects: $e, keep: $keep]")
        keep
      case sym =>
        log(c"  ${str(sym)} [Was pure in context list?]")
        false
    }
    log("<End of context>")

    val basicBlock = if (state.useBasicBlocks) state.currentBlock.reverse.distinct else Nil

    state.context = if (saveContext ne null) saveContext ++ remain else null // let all other effects go if we've reached end of program
    state.currentBlock = saveBlock
    state.blockEffects = saveEffects

    // Reset contents of defCache
    // -- prevents CSEing across inescapable blocks
    if (isolated || seal || temp == Freq.Cold) state.defCache = saveCache

    val effects = summarizeScope(deps)
    (result, effects, deps, basicBlock)
  }

  @stateful def stageProgramBlock[R](block: => Exp[R]): Block[R] = {
    val properties = Block.Program
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Block(Nil, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }

  @stateful def stageBlock[R](block: => Exp[R], properties: BlockProperties = Block.Normal): Block[R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Block(Nil, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambda1[A,R](a: Exp[A])(block: => Exp[R], properties: BlockProperties = Block.Normal): Lambda1[A,R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Lambda1(a, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R], properties: BlockProperties = Block.Normal): Lambda2[A,B,R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Lambda2(a, b, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R], properties: BlockProperties = Block.Normal): Lambda3[A,B,C,R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Lambda3(a, b, c, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R], properties: BlockProperties = Block.Normal): Lambda4[A,B,C,D,R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Lambda4(a, b, c, d, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambda5[A,B,C,D,E,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E])(block: => Exp[R], properties: BlockProperties = Block.Normal): Lambda5[A,B,C,D,E,R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Lambda5(a, b, c, d, e, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambda6[A,B,C,D,E,F,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F])(block: => Exp[R], properties: BlockProperties = Block.Normal): Lambda6[A,B,C,D,E,F,R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Lambda6(a, b, c, d, e, f, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }
  @stateful def stageLambdaN[R](inputs: Seq[Exp[_]], block: => Exp[R], properties: BlockProperties = Block.Normal): Block[R] = {
    val (result, effects, effectful, bb) = stageScope(block, properties)
    val blk = Block(inputs, result, effects, effectful, properties)
    if (state.useBasicBlocks) state.basicBlocks(blk) = bb
    blk
  }

  @stateful def stageIsolatedBlock[T](block: => Exp[T]): Block[T] = stageBlock[T](block, Block.Isolated)

  @stateful def stageColdBlock[R](block: => Exp[R]) = stageBlock(block, Block.Cold)
  @stateful def stageColdLambda1[A,R](a: Exp[A])(block: => Exp[R]) = stageLambda1(a)(block, Block.Cold)
  @stateful def stageColdLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]) = stageLambda2(a,b)(block, Block.Cold)
  @stateful def stageColdLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R])= stageLambda3(a,b,c)(block, Block.Cold)
  @stateful def stageColdLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R])= stageLambda4(a,b,c,d)(block, Block.Cold)
  @stateful def stageColdLambda5[A,B,C,D,E,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E])(block: => Exp[R])= stageLambda5(a,b,c,d,e)(block, Block.Cold)
  @stateful def stageColdLambda6[A,B,C,D,E,F,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F])(block: => Exp[R])= stageLambda6(a,b,c,d,e,f)(block, Block.Cold)

  @stateful def stageHotBlock[R](block: => Exp[R]) = stageBlock(block, Block.Hot)
  @stateful def stageHotLambda1[A,R](a: Exp[A])(block: => Exp[R]) = stageLambda1(a)(block, Block.Hot)
  @stateful def stageHotLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]) = stageLambda2(a,b)(block, Block.Hot)
  @stateful def stageHotLambda3[A,B,C,R](a: Exp[A], b: Exp[B], c: Exp[C])(block: => Exp[R])= stageLambda3(a,b,c)(block, Block.Hot)
  @stateful def stageHotLambda4[A,B,C,D,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D])(block: => Exp[R])= stageLambda4(a,b,c,d)(block, Block.Hot)
  @stateful def stageHotLambda5[A,B,C,D,E,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E])(block: => Exp[R])= stageLambda5(a,b,c,d,e)(block, Block.Hot)
  @stateful def stageHotLambda6[A,B,C,D,E,F,R](a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F])(block: => Exp[R])= stageLambda6(a,b,c,d,e,f)(block, Block.Hot)

  @stateful def stageSealedBlock[R](block: => Exp[R]): Block[R] = stageBlock[R](block, Block.Sealed)
  @stateful def stageSealedLambda1[A,R](a: Exp[A])(block: => Exp[R]): Lambda1[A,R] = stageLambda1[A,R](a)(block, Block.Sealed)
  @stateful def stageSealedLambda2[A,B,R](a: Exp[A], b: Exp[B])(block: => Exp[R]): Lambda2[A,B,R] = stageLambda2[A,B,R](a,b)(block, Block.Sealed)

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
