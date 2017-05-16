package argon.core

import scala.collection.mutable


trait Scheduling extends Statements { this: Staging =>

  val scopeCache = mutable.HashMap[Block[_],Seq[Stm]]()

  def makeScopeIndex(scope: Iterable[Stm]): OrderCache = buildScopeIndex(scope.map(_.rhs.id))
  def orderedInputs(roots: Iterable[Exp[_]], cache: OrderCache): Seq[Stm] = {
    scheduleDepsWithIndex(dyns(roots).map(_.id), cache).flatMap(stmFromNodeId)
  }

  def schedule(roots: Iterable[Stm], checkAcyclic: Boolean = true)(next: Exp[_] => Seq[Stm]): Seq[Stm] = {
    def succ(node: NodeId): Iterable[NodeId] = nodeOutputs(node).map(symFromSymId).flatMap(next).map(_.rhs.id)

    val start = roots.map(_.rhs.id)

    val ids = if (checkAcyclic) {
      val xx = sccs(start){node => succ(node) }
      checkIfAcyclic(roots, xx)
      xx.flatten.reverse
    }
    else {
      dfs(start){node => succ(node) }
    }
    ids.flatMap(stmFromNodeId)
  }

  def scheduleBlock(availNodes: Seq[NodeId], block: Block[_]): Seq[Stm] = scopeCache.getOrElseUpdate(block, {
    val allDependencies = dyns(block.result +: block.effectful)
    val result = allDependencies.map(_.id)

    /**
      * Check that nothing got messed up in the IR
      * (Basically just checks that, for a given block, all effects that are bound to that block were actually scheduled)
      */
    def scopeSanityCheck(block: Block[_], scope: Seq[NodeId]): Unit = {
      val observable = block.effectful.map(defOf).map(_.id).distinct // node ids for effect producers
      val actual = observable intersect scope
      val missing = observable diff actual
      if (missing.nonEmpty) {
        val expectedStms = observable.flatMap{s => stmFromNodeId(s)}
        val actualStms = actual.flatMap{s => stmFromNodeId(s)}
        val missingStms = missing.flatMap{s => stmFromNodeId(s)}
        val binding = missing.flatMap{id =>
          stmFromNodeId(id).map {stm =>
            val binders = getNodesThatBindGiven(availNodes, result, Seq(id))
            stm -> binders.flatMap(stmFromNodeId)
          }
        }
        throw new EffectsOrderException(block.result, expectedStms, actualStms, missingStms, binding)
      }
    }

    // Result and scheduling dependencies
    val schedule = getLocalSchedule(availableNodes = availNodes, result = result)
    scopeSanityCheck(block, schedule)

    schedule.flatMap{nodeId => stmFromNodeId(nodeId) }
  })

  override def reset(): Unit = {
    super.reset()
    scopeCache.clear()
  }

}
