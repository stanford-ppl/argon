package argon.core

import scala.collection.mutable

trait Blocks extends Effects { self: Statements =>
  type CompilerPass = Traversal{val IR: Blocks.this.type }

  // --- State
  private[argon] var eX: Option[CompilerPass] = None

  /**
    * Class representing the result of a staged scope.
    * @param __result: symbolic result of the scope
    * @param summary: effects summary for the entire scope
    * @param effects: list of all symbols with effectful nodes in the scope
    */
  case class Block[S:Typ](private[Blocks] val __result: S, summary: Effects, effects: List[Sym]) {
    val tp: Typ[_] = typ[S]
    def result: S = if (eX.isDefined) eX.get.run(this).__result else __result

    def getResult: S = __result
  }

  /**
    * Computes an *external* summary for a seq of nodes
    * (Ignores reads/writes on data allocated within the scope)
    */
  def summarizeScope(context: List[Sym]): Effects = {
    var effects = Pure
    val allocs = new mutable.HashSet[Sym]
    def clean(xs: Set[Sym]) = xs diff allocs
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
  def stageScope[S<:Sym:Typ](block: => S): Block[S] = {
    val saveContext = context
    context = Nil

    val result = block
    val deps = context
    context = saveContext

    val effects = summarizeScope(deps)
    Block[S](result, effects, deps)
  }
  /**
    * Stage the effects of a block that is executed 'here' (if it is executed at all).
    * All assumptions about the current context carry over unchanged.
    */
  def stageScopeInline[S<:Sym:Typ](block: => S): Block[S] = {
    val saveContext = context
    if (saveContext eq null) context = Nil

    val result = block
    val nAdded = context.length - saveContext.length

    if ((saveContext ne null) && context.drop(nAdded) != saveContext)
      throw IllegalStageHereException(saveContext, context)

    val deps = if (saveContext eq null) context else context.take(nAdded) // Most recent effects

    val effects = summarizeScope(deps)
    context = saveContext

    Block[S](result, effects, deps)
  }

  /** Compiler debugging **/
  override def readable(x: Any) = x match {
    case b: Block[_] => c"Block(${b.getResult})"
    case _ => super.readable(x)
  }
}
