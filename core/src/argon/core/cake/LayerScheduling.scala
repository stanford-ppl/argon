package argon.core.cake

import forge._

trait LayerScheduling { this: ArgonCake =>

  @stateful def makeScopeIndex(scope: Iterable[Stm])(implicit state: State): OrderCache = {
    state.graph.buildScopeIndex(scope.map(_.rhs.id))
  }
  @stateful def orderedInputs(roots: Iterable[Exp[_]], cache: OrderCache): Seq[Stm] = {
    state.graph.scheduleDepsWithIndex(dyns(roots).map(_.id), cache).flatMap(stmFromNodeId)
  }

  @stateful def schedule(roots: Iterable[Stm], checkAcyclic: Boolean = true)(next: Exp[_] => Seq[Stm]): Seq[Stm] = {

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

  @stateful def scheduleBlock(availNodes: Seq[NodeId], block: Block[_]): Seq[Stm] = state.scopeCache.getOrElseUpdate(block, {
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
            val binders = state.graph.getNodesThatBindGiven(availNodes, result, Seq(id))
            stm -> binders.map{case (node, parent2, bound) => (stmFromNodeId(node), parent2.flatMap(stmFromNodeId), bound.flatMap(stmFromNodeId)) }
          }
        }
        error(c"Violated ordering of effects while traversing block result: ")
        error(c"${str(block.result)}")
        error("expected: ")
        expectedStms.foreach{stm => error(c"  $stm")}
        error("actual: ")
        actualStms.foreach{stm => error(c"  $stm")}
        error("missing: ")
        //missing.foreach{stm => error(c"  $stm")}
        binding.foreach { case (stm, bindedby) =>
          dbg(c"  $stm")
          dbg("  appears to be unschedulable because of: ")
          bindedby.foreach{
            case (Some(node), p2, bound) =>
              if (p2.isDefined) dbg("  The pair of nodes:") else dbg("  The node:")
              dbg(c"    $node")
              strMeta(node.lhs.head)
              p2.foreach{otherNode =>
                dbg(c"    $otherNode")
                strMeta(otherNode.lhs.head)
              }
              bound.foreach{bnd =>
                dbg("  Binding: ")
                dbg(c"    $bnd")
                strMeta(bnd.lhs.head)
              }
            case _ =>
          }
          dbg("")
          dbg("")
        }
        state.logError()
      }
    }

    // Result and scheduling dependencies
    val schedule = state.graph.getLocalSchedule(availableNodes = availNodes, result = result)
    scopeSanityCheck(block, schedule)

    schedule.flatMap{nodeId => stmFromNodeId(nodeId) }
  })
}
