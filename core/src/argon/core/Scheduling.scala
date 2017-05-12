package argon.core

import argon._
import forge._

trait Scheduling { this: ArgonCore =>
  // TODO: Awkward placement for this - better spot?
  // Based on performance testing LongMap is slightly faster than others (even with casting)
  type OrderCache = scala.collection.mutable.LongMap[(NodeId,Int)] // EdgeId -> (NodeId,Int)
  def OrderCache() = new scala.collection.mutable.LongMap[(NodeId,Int)]()

  @stateful def makeScopeIndex(scope: Iterable[Stm])(implicit state: State): OrderCache = {
    state.graph.buildScopeIndex(scope.map(_.rhs.id))
  }
  @stateful def orderedInputs(roots: Iterable[Exp[_]], cache: OrderCache)(implicit state: State): Seq[Stm] = {
    state.graph.scheduleDepsWithIndex(dyns(roots).map(_.id), cache).flatMap(stmFromNodeId)
  }

  @stateful def schedule(roots: Iterable[Stm], checkAcyclic: Boolean = true)(next: Exp[_] => Seq[Stm])(implicit state: State): Seq[Stm] = {

    def succ(node: NodeId): Iterable[NodeId] = state.graph.nodeOutputs(node).map(symFromSymId).flatMap(next).map(_.rhs.id)

    val start = roots.map(_.rhs.id)

    val ids = if (checkAcyclic) {
      val xx = state.graph.sccs(start){node => succ(node) }
      state.graph.checkIfAcyclic(roots, xx)
      xx.flatten.reverse
    }
    else {
      state.graph.dfs(start){node => succ(node) }
    }
    ids.flatMap(stmFromNodeId)
  }

  @stateful def scheduleBlock(availNodes: Seq[NodeId], block: Block[_])(implicit state: State): Seq[Stm] = state.scopeCache.getOrElseUpdate(block, {
    /**
      * Check that nothing got messed up in the IR
      * (Basically just checks that, for a given block, all effects that should be bound to that block were scheduled)
      */
    def scopeSanityCheck(block: Block[_], scope: Seq[NodeId]): Unit = {
      val observable = block.effectful.map(defOf).map(_.id).distinct // node ids for effect producers
      val actual = observable intersect scope
      val missing = observable diff actual
      if (missing.nonEmpty) {
        val expectedStms = observable.flatMap{s => stmFromNodeId(s)}
        val actualStms = actual.flatMap{s => stmFromNodeId(s)}
        val missingStms = missing.flatMap{s => stmFromNodeId(s)}
        throw new EffectsOrderException(block.result, expectedStms, actualStms, missingStms)
      }
    }

    val allDependencies = dyns(block.result +: block.effectful)
    // Result and scheduling dependencies
    val schedule = state.graph.getLocalSchedule(availableNodes = availNodes, result = allDependencies.map(_.id))
    scopeSanityCheck(block, schedule)

    schedule.flatMap{nodeId => stmFromNodeId(nodeId) }
  })
}
