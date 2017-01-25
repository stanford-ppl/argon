package argon.graphs

import argon.core.Exceptions
import argon.core.Reporting

import scala.collection.mutable

trait AbstractHDAG extends Exceptions {
  type EdgeId = Int
  type NodeId = Int
  type Triple = (Iterable[Edge], Node, Iterable[Edge]) // outputs, node, inputs

  var curEdgeId : EdgeId = 0
  var curNodeId : NodeId = 0

  var curInputId: EdgeId = 0

  /** Recursive (cyclic) graphs are unsupported **/
  def checkIfAcyclic(root: Any, components: List[List[NodeId]]) = components.foreach{x =>
    if (x.length > 1) {
      val stms = x.map{nodeId => triple(nodeId) }.map{x => c"${x._1} = ${x._2}${x._3}"}
      throw new RecursiveScheduleException(root, stms)
    }
  }


  object EdgeData {
    val value = mutable.ArrayBuffer[Edge]()
    val producer = mutable.ArrayBuffer[NodeId]()            // Index of node corresponding to this edge
    val dependents = mutable.ArrayBuffer[List[NodeId]]()    // Dataflow edges (forward) -- per edge
  }

  object NodeData {
    val value   = mutable.ArrayBuffer[Node]()
    val outputs = mutable.ArrayBuffer[EdgeId](0)            // nodeId :: nodeId+1 ==> edges for this node
    val inputs  = mutable.ArrayBuffer[List[EdgeId]]()       // Dataflow edges (reverse) -- per node
    val bounds  = mutable.ArrayBuffer[List[EdgeId]]()       // Edges bound by this node
    val tunnels = mutable.ArrayBuffer[List[EdgeId]]()       // Edges bound by this node, defined elsewhere
    val freqs   = mutable.ArrayBuffer[List[Float]]()        // Frequency hints for code motion
  }

  def nodeOutputs(node: NodeId) = NodeData.outputs(node) until NodeData.outputs(node+1)
  def nodeInputs(node: NodeId): List[EdgeId] = NodeData.inputs(node)
  def nodeBounds(node: NodeId): List[EdgeId] = NodeData.bounds(node)
  def nodeTunnels(node: NodeId): List[EdgeId] = NodeData.tunnels(node)
  def nodeFreqs(node: NodeId): List[Float] = NodeData.freqs(node)
  def nodeOf(node: NodeId): Node = NodeData.value(node)
  def edgeOf(edge: EdgeId): Edge = EdgeData.value(edge)
  def dependentsOf(edge: EdgeId): List[NodeId] = EdgeData.dependents(edge)
  def producerOf(edge: EdgeId): NodeId = EdgeData.producer(edge)
  def triple(id: NodeId): Triple = (nodeOutputs(id).map(edgeOf), nodeOf(id), nodeInputs(id).map(edgeOf))

  def removeEdge(in: EdgeLike, remNode: Boolean): Unit = {
    EdgeData.dependents(in._id) = Nil
    if (remNode) removeNode(producerOf(in._id))
  }
  def removeNode(id: NodeId): Unit = {
    NodeData.value(id) = null
    NodeData.outputs(id) = NodeData.outputs(id - 1)
    NodeData.inputs(id) = Nil
    NodeData.bounds(id) = Nil
    NodeData.tunnels(id) = Nil
    NodeData.freqs(id) = Nil
  }

  /** Add an edge with no node or scheduling dependencies **/
  final def registerInput(in: EdgeLike): Unit = {
    in.id_=(curInputId)
    curInputId += 1
  }

  /** Add an edge with no node but which may be required for scheduling **/
  final def addBound(out: Edge) = addNode(Nil, List(out), Nil, Nil, Nil, null).head

  /** Add output edge(s) with corresponding node **/
  final def addNode(ins: List[Edge], outs: List[Edge], binds: List[Edge], tunnel: List[Edge], freqs: List[Float], node: Node) = {
    outs.foreach { out =>
      out.id = curEdgeId

      EdgeData.value += out
      EdgeData.producer += curNodeId
      EdgeData.dependents += Nil

      curEdgeId += 1
      out
    }
    ins.foreach { in => EdgeData.dependents(in.id) ::= curNodeId }

    if (node != null) node.id = curNodeId
    NodeData.value += node
    NodeData.outputs += curEdgeId
    NodeData.inputs += ins.map(_.id)
    NodeData.bounds += binds.map(_.id)
    NodeData.tunnels += tunnel.map(_.id)
    NodeData.freqs += freqs
    curNodeId += 1

    outs
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
    * Get all nodes which must be bound in the given scope
    *   - bound symbols: ALL dependents, stopping at binders,
    *   - tunnel symbols: ANY dependents of tunnel symbols which are also dependencies of the binder
    * Note that while it would be nice to only do one DFS, we need to track which nodes are bound where.
    */
  def getBoundDependents(scope: Set[NodeId]): Set[NodeId] = {
    // Get all dependencies of root, stopping at nodes which bound it
    def getDependents(root: NodeId) = {
      dfs(List(root)){node => forward(node, scope) filterNot (nodeBounds(_) contains root)}
    }

    val boundDependents = scope.flatMap{node =>
      val bounds = nodeBounds(node).map(producerOf)
      val sched = bounds.flatMap(getDependents)

//      if (sched.nonEmpty) {
//        log(c"  node: ${triple(node)}")
//        log(c"  bounds:")
//        bounds.foreach{bnd => log(c"    ${triple(bnd)}")}
//        log(c"  binded: ")
//        sched.foreach{s => log(c"    ${triple(s)}") }
//      }
      sched
    }
    val tunnelDependents = scope.flatMap{node =>
      val tunnels = nodeTunnels(node).map(producerOf)
      val schedule = dfs(List(node)){node => reverse(node, scope) } filterNot(_ == node)
      val tunnelForward = tunnels.flatMap(getDependents) diff tunnels
//      if (tunnels.nonEmpty) {
//        log(c"  Computing tunnel dependencies: ")
//        log(c"  ${triple(node)}: ")
//        log(c"  tunnels: ")
//        tunnels.foreach{stm => log(c"    ${triple(stm)}")}
//        log(c"  schedule: ")
//        schedule.foreach{stm => log(c"    ${triple(stm)}")}
//        log(c"  tunnel dependents: ")
//        tunnelForward.foreach{stm => log(c"    ${triple(stm)}")}
//      }

      tunnelForward intersect schedule
    }
    boundDependents ++ tunnelDependents
  }

  def getLocalScope(currentScope: Seq[NodeId], result: List[EdgeId]): Seq[NodeId] = {
    val scope = currentScope.toSet

    val localCache = buildScopeIndex(scope)
    val roots = scheduleDepsWithIndex(result, localCache)

    val mustInside = getBoundDependents(scope)

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

    // ISSUE #1: Investigate reverted version of LMS code motion
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
