package argon.core
package cake

import argon.Config
import forge._

trait LayerStatements { this: ArgonCore =>
  type NodeId = Int
  type EdgeId = Int

  // --- Helper functions
  // Getting statement returns Option to account for Bounds, but this is known to be a Sym
  @stateful def stmOf(sym: Sym[_]): Stm = stmFromSymId(sym.id).get

  @stateful def stmFromNodeId(id: Int): Option[Stm] = {
    val x = state.graph.triple(id)
    x._1.head match {
      case _:Sym[_] =>
        val lhs = x._1.toList.map(_.asInstanceOf[Sym[_]])
        val rhs = x._2
        Some(Stm(lhs, rhs))

      case _:Bound[_] => None
    }
  }
  @stateful def stmFromSymId(id: EdgeId): Option[Stm] = {
    val node = state.graph.producerOf(id)
    stmFromNodeId(node)
  }

  @stateful def symFromSymId(id: EdgeId): Dyn[_] = state.graph.edgeOf(id)
  @stateful def defFromNodeId(id: NodeId): Def = state.graph.nodeOf(id)
  @stateful def defFromSymId(id: EdgeId): Option[Def] = stmFromSymId(id).map(_.rhs)

  // --- Symbol aliasing
  private def noPrims(x: Set[Dyn[_]]): Set[Sym[_]] = x.collect{case s: Sym[_] if !s.tp.isPrimitive => s}

  @stateful def shallowAliases(x: Any): Set[Sym[_]] = {
    noPrims(aliasSyms(x)).flatMap { case s@Def(d) => state.shallowAliasCache.getOrElseUpdate(s, shallowAliases(d)) + s } ++
      noPrims(extractSyms(x)).flatMap { case s@Def(d) => state.deepAliasCache.getOrElseUpdate(s, deepAliases(d)) }
  }
  @stateful def deepAliases(x: Any): Set[Sym[_]] = {
    noPrims(aliasSyms(x)).flatMap { case s@Def(d) => state.deepAliasCache.getOrElseUpdate(s, deepAliases(d)) } ++
      noPrims(copySyms(x)).flatMap { case s@Def(d) => state.deepAliasCache.getOrElseUpdate(s, deepAliases(d)) } ++
      noPrims(containSyms(x)).flatMap { case s@Def(d) => state.aliasCache.getOrElseUpdate(s, allAliases(d)) + s } ++
      noPrims(extractSyms(x)).flatMap { case s@Def(d) => state.deepAliasCache.getOrElseUpdate(s, deepAliases(d)) }
  }
  @stateful final def allAliases(x: Any): Set[Sym[_]] = {
    shallowAliases(x) ++ deepAliases(x)
  }
  @stateful final def mutableAliases(x: Any): Set[Sym[_]] = allAliases(x).filter(isMutable)
  @stateful final def mutableInputs(d: Def): Set[Sym[_]] = {
    val bounds = d.binds
    val actuallyReadSyms = d.reads diff bounds
    mutableAliases(actuallyReadSyms) filterNot (bounds contains _)
  }

  /** Used to allow nested ("atomic") writes, which are reflected on the top mutable object rather than intermediates
    * e.g.
    *   val b = Array(1, 2, 3)
    *   val a = MutableStruct(b, ...)
    *   a.b(0) = 1
    * Should become a write on (the mutable symbol) a instead of the immutable symbol resulting from a.b
    *
    * TODO: Any reason for this to be Sym[_] => Seq[Sym[_]] ?
    */
  @stateful final def recurseAtomicLookup(e: Exp[_]): Exp[_] = {
    getDef(e).flatMap{case d: AtomicRead[_] => Some(d.coll); case _ => None}.getOrElse(e)
  }
  @stateful final def extractAtomicWrite(s: Sym[_]): Sym[_] = {
    syms(recurseAtomicLookup(s)).headOption.getOrElse(s)
  }

  @stateful final def propagateWrites(effects: Effects): Effects = if (!Config.allowAtomicWrites) effects else {
    val writes = effects.writes.map{s => extractAtomicWrite(s) }
    effects.copy(writes = writes)
  }
}
