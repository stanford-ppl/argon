package argon.core

import argon.traversal.CompilerPass
import scala.collection.mutable

trait Blocks extends Effects { self: Staging =>
  type Pass = CompilerPass{ val IR: self.type }
  var blockEffects: Effects = Pure

  /** Class representing the result of a staged scope. */
  case class Block[+T](result: Exp[T], summary: Effects, effectful: Seq[Sym[_]], inputs: Seq[Sym[_]], temp: UseFreq) {
    def tp: Type[_] = result.tp
  }

  /**
    * Computes an *external* summary for a seq of nodes
    * (Ignores reads/writes on data allocated within the scope)
    */
  def summarizeScope(context: Seq[Sym[_]]): Effects = {
    var effects = Pure
    val allocs = new mutable.HashSet[Sym[_]]
    def clean(xs: Set[Sym[_]]) = xs diff allocs
    for (s@Effectful(u2, _) <- context) {
      if (u2.isMutable) allocs += s
      effects = effects andThen u2.copy(reads = clean(u2.reads), writes = clean(u2.writes))
    }
    effects
  }

  def createBlock[T:Type](block: => Exp[T], inputs: Seq[Sym[_]], temp: UseFreq, isolated: Boolean = false): Block[T] = {
    val saveContext = context
    val saveCache = defCache
    context = Nil

    // In an isolated block, don't allow CSE with outside statements either
    if (isolated) defCache = Map.empty

    val result = block
    val deps = context
    context = saveContext

    // Reset contents of defCache when staging cold blocks
    // Reasoning here is that statements can't move out of cold blocks, so it makes no sense to CSE across them
    if (temp == Freq.Cold) defCache = saveCache

    val effects = summarizeScope(deps)
    Block[T](result, effects, deps, inputs, temp)
  }

  /**
    * Stage the effects of an isolated block.
    * No assumptions about the current context remain valid.
    */
  def stageIsolatedBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Cold, isolated = true)

  def stageSealedBlock[T:Type](block: => Exp[T]): Block[T] = {
    var prevEffects = blockEffects
    blockEffects = Cold
    val result = createBlock[T](block, Nil, Freq.Normal)
    blockEffects = prevEffects
    result
  }

  def stageBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Normal)
  def stageLambda[T:Type](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, syms(inputs), Freq.Normal)

  def stageColdBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Cold)
  def stageColdLambda[T:Type](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, syms(inputs), Freq.Cold)

  def stageHotBlock[T:Type](block: => Exp[T]): Block[T] = createBlock[T](block, Nil, Freq.Hot)
  def stageHotLambda[T:Type](inputs: Exp[_]*)(block: => Exp[T]): Block[T] = createBlock[T](block, syms(inputs), Freq.Hot)


  /**
    * Stage the effects of a block that is executed 'here' (if it is executed at all).
    * All assumptions about the current context carry over unchanged.
    */
  def stageBlockInline[T:Type](block: => Exp[T]): Block[T] = {
    val saveContext = context
    if (saveContext eq null) context = Nil

    val result = block
    val nAdded = context.length - saveContext.length

    if ((saveContext ne null) && context.drop(nAdded) != saveContext)
      throw IllegalStageHereException(saveContext, context)

    val deps = if (saveContext eq null) context else context.take(nAdded) // Most recent effects

    val effects = summarizeScope(deps)
    context = saveContext

    Block[T](result, effects, deps, Nil, Freq.Normal)
  }

  /** Compiler debugging **/
  override def readable(x: Any) = x match {
    case b: Block[_] if b.inputs.isEmpty => c"Block(${b.result})"
    case b: Block[_] => c"""Block(${b.inputs.mkString("(",",",")")} => ${b.result})"""
    case _ => super.readable(x)
  }
}
