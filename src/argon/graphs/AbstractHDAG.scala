package argon.graphs

import argon.core.CompilerException
import argon.core.Reporting

import scala.collection.mutable
import scala.language.higherKinds

abstract class AbstractHDAG[EdgeId,NodeId,Edge,Node](implicit elike: EdgeLike[EdgeId,Edge], nlike: NodeLike[NodeId,Node]) extends Reporting {
  import elike._
  import nlike._

  class RecursiveScheduleException(result: Any, xs: List[String]) extends
    CompilerException(c"Recursive schedule while scheduling result $result", {
      error(c"Recursive schedule while scheduling result $result:")
      xs.foreach{x => error(s"  $x") }
    })

  type Triple = (Iterable[Edge], Node, Iterable[Edge]) // outputs, node, inputs

  /** Recursive (cyclic) graphs are unsupported **/
  def checkIfAcyclic(root: Any, components: List[List[NodeId]]) = components.foreach{x =>
    if (x.length > 1) {
      val stms = x.map{nodeId => triple(nodeId) }.map{x => c"${x._1} = ${x._2}${x._3}"}
      throw new RecursiveScheduleException(root, stms)
    }
  }

  val firstEdgeId:EdgeId
  var curEdgeId:EdgeId
  var curNodeId:NodeId
  def nextEdgeId():Unit
  def nextNodeId():Unit

  object EdgeData {
    val value = mutable.ArrayBuffer[Edge]()
    val producer = mutable.ArrayBuffer[NodeId]()            // Index of node corresponding to this edge
    val dependents = mutable.ArrayBuffer[List[NodeId]]()    // Dataflow edges (forward) -- per edge
  }

  object NodeData {
    val value   = mutable.ArrayBuffer[Node]()
    val outputs = mutable.ArrayBuffer[EdgeId](firstEdgeId)  // nodeId :: nodeId+1 ==> edges for this node
    val inputs  = mutable.ArrayBuffer[List[EdgeId]]()       // Dataflow edges (reverse) -- per node
    val bounds  = mutable.ArrayBuffer[List[EdgeId]]()       // Edges bound by this node
    val freqs   = mutable.ArrayBuffer[List[Float]]()        // Frequency hints for code motion
  }

  def addDependent(edge:EdgeId, dependent:NodeId): Unit
  def nodeOutputs(id:NodeId):Iterable[EdgeId]
  def nodeInputs(node: NodeId): List[EdgeId]
  def nodeBounds(node: NodeId): List[EdgeId]
  def nodeFreqs(node: NodeId): List[Float]
  def nodeOf(id: NodeId): Node
  def edgeOf(id: EdgeId): Edge
  def dependentsOf(id: EdgeId): List[NodeId]
  def producerOf(id: EdgeId): NodeId
  def triple(id: NodeId): Triple = (nodeOutputs(id).map(edgeOf), nodeOf(id), nodeInputs(id).map(edgeOf))

  final def addNode(inputs: List[(Edge,Float)], outputs:List[Edge], binds: List[Edge], node: Node) {
    outputs.foreach { out =>
      out.id = curEdgeId
      EdgeData.value += out
      EdgeData.producer += curNodeId
      EdgeData.dependents += Nil
      nextEdgeId()
    }
    val ins = inputs.map(_._1)
    ins.foreach { in => addDependent(in.id, curNodeId) }

    node.id = curNodeId
    NodeData.value += node
    NodeData.outputs += curEdgeId
    NodeData.inputs += ins.map(_.id)
    NodeData.bounds += binds.map(_.id)
    NodeData.freqs += inputs.map(_._2)

    nextNodeId()
  }

  // --- Scheduling
  type OrderCache

  def buildScopeIndex(scope: Iterable[NodeId]): OrderCache
  def scheduleDepsWithIndex(syms: Iterable[EdgeId], cache: OrderCache): List[NodeId]

  def sccs(start: Iterable[NodeId])(succ: NodeId => Iterable[NodeId]): List[List[NodeId]]
  def dfs(start: Iterable[NodeId])(succ: NodeId => Iterable[NodeId]): List[NodeId]

  // Visit methods (scoped)
  def ordered(node: NodeId, cache: OrderCache): Iterable[NodeId]
  = scheduleDepsWithIndex(nodeInputs(node), cache)
  def reverse(node: NodeId, scope: Set[NodeId]): Iterable[NodeId]
  = nodeInputs(node).map(producerOf) filter scope.contains
  def forward(node: NodeId, scope: Set[NodeId]): Iterable[NodeId]
  = nodeOutputs(node).flatMap(dependentsOf) filter scope.contains
  def noCold(node: NodeId, scope: Set[NodeId]): Iterable[NodeId]
  = nodeInputs(node).zip(nodeFreqs(node)).filter(_._2 > 0.75f).map(x => producerOf(x._1)) filter scope.contains
  def noHot(node: NodeId, scope: Set[NodeId]): Iterable[NodeId]
  = nodeInputs(node).zip(nodeFreqs(node)).filter(_._2 < 100f).map(x => producerOf(x._1)) filter scope.contains

  // Visit methods (unscoped)
  def reverse(node: NodeId): Iterable[NodeId] = nodeInputs(node).map(producerOf)
  def forward(node: NodeId): Iterable[NodeId] = nodeOutputs(node).flatMap(dependentsOf)

  def getSchedule(roots: Iterable[NodeId], cache: OrderCache, checkAcyclic: Boolean = true): List[NodeId] = {
    if (checkAcyclic) {
      val xx = sccs(roots){node => ordered(node, cache) }
      checkIfAcyclic(roots, xx)
      xx.flatten.reverse
    }
    else {
      dfs(roots){node => ordered(node, cache) }
    }
  }
  /**
    * Get all nodes which depend on the given bound nodes, stopping DFS when reaching nodes which bind them
    * Note that while it would be nice to only do one DFS, we need to track which nodes are bound where.
    */
  def getBoundDependents(roots: Iterable[NodeId], scope: Set[NodeId]): Set[NodeId] = {
    // Get all dependencies of root, stopping at nodes which bound it
    def getDependents(root: NodeId) = {
      dfs(List(root)){node => forward(node, scope) filterNot (nodeBounds(_) contains root)}
    }
    roots.flatMap(getDependents).toSet
  }


  def getLocalScope(currentScope: Seq[NodeId], result: List[EdgeId]): Seq[NodeId] = {
    val scope = currentScope.toSet

    val localCache = buildScopeIndex(scope)
    val roots = scheduleDepsWithIndex(result, localCache)

    val bounds = scope.flatMap{node => nodeBounds(node) }.map(producerOf)
    val mustInside = getBoundDependents(bounds, scope)

    val mayOutside = scope diff mustInside
    val fringe = mustInside.flatMap(reverse) intersect mayOutside

    // 2. Statements that can be reached without following any cold/hot inputs, respectively
    val reachableWarm = dfs(roots)(node => noCold(node,scope)).toSet
    val reachableCold = dfs(roots)(node => noHot(node,scope)).toSet

    // 3. All symbols s such that all paths from results to s are hot paths
    val hotPathsOnly = reachableWarm diff reachableCold

    // 4. Dependencies of symbols on hot paths
    val hotDeps = dfs(hotPathsOnly)(node => reverse(node,scope)).toSet

    // 5. Hot dependencies on the fringe
    val hotFringe = fringe intersect hotDeps

    // 5. Dependencies of the hot fringe
    val hotFringeDeps = dfs(hotFringe)(node => noCold(node, scope)).toSet

    // ISSUE #2: Investigate reverted version of LMS code motion
    // use (shallow|hot)* hot any* instead
    // then/else branches were being unsafely hoisted out of a conditional
    //val f2 = fringe diff reachableCold
    //val h2 = getFreqSchedule(scope)(f2.map(_.lhs), cold=false, hot=true)
    //def shouldOutside(x: Stm) = reachableWarm.contains(x) || h2.contains(x)

    def shouldOutside(x: NodeId) = reachableWarm.contains(x) || hotFringeDeps.contains(x)
    def canOutside(x: NodeId) = shouldOutside(x) && !mustInside.contains(x)

    val levelScope = currentScope.filter(canOutside)

//    log("Getting scope level within scope: ")
//    scope.foreach{stm => log(c"  ${triple(stm)}") }
//    log("For result: ")
//    result.foreach{s => log(c"  ${triple(producerOf(s))}") }
//    log("Must inside: ")
//    mustInside.foreach{stm => log(c"  ${triple(stm)}")}
//    log("May outside: ")
//    mayOutside.foreach{stm => log(c"  ${triple(stm)}")}
//    log("Fringe: ")
//    fringe.foreach{stm => log(c"  ${triple(stm)}") }
//    log("Reachable [Warm]: ")
//    reachableWarm.foreach{stm => log(c"  ${triple(stm)}") }
//    log("Reachable [Cold]: ")
//    reachableCold.foreach{stm => log(c"  ${triple(stm)}")}
//    log(s"Hot paths: ")
//    hotPathsOnly.foreach{stm => log(c"  ${triple(stm)}")}
//    log(s"Hot dependencies: ")
//    hotDeps.foreach{stm => log(c"  ${triple(stm)}")}
//    log(s"Hot fringe: ")
//    hotFringe.foreach{stm => log(c"  ${triple(stm)}")}
//    log("Hot fringe dependencies: ")
//    hotFringeDeps.foreach{stm => log(c"  ${triple(stm)}")}
//    log("levelScope: ")
//    levelScope.foreach{stm => log(c"  ${triple(stm)}")}

    levelScope
  }


  def getLocalSchedule[A](availableNodes: Seq[NodeId], result: List[EdgeId]) = {
    val availCache = buildScopeIndex(availableNodes)
    val availRoots = scheduleDepsWithIndex(result, availCache)
    val localNodes = getSchedule(availRoots, availCache, checkAcyclic = true)

    val localScope = getLocalScope(localNodes, result)
    val localCache = buildScopeIndex(localScope)
    val localRoots = scheduleDepsWithIndex(result, localCache)
    val localSchedule = getSchedule(localRoots, localCache, checkAcyclic = false)

//    log("avail nodes: ")
//    availableNodes.foreach{stm => log(c"  ${triple(stm)}")}
//    log("avail roots:")
//    availRoots.foreach{stm => log(c"  ${triple(stm)}")}
//    log("local nodes:")
//    localNodes.foreach{stm => log(c"  ${triple(stm)}")}
//    log("local scope: ")
//    localScope.foreach{stm => log(c"  ${triple(stm)}")}
//    log("local roots:")
//    localRoots.foreach{stm => log(c"  ${triple(stm)}")}
//    log("local schedule:")
//    localSchedule.foreach{stm => log(c"  ${triple(stm)}")}

    localSchedule
  }
}
