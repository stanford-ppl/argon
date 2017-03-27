package argon.graphs

import argon.core.Exceptions
import scala.collection.mutable

trait Graph extends Exceptions {
  type EdgeId = Int
  type NodeId = Int

  implicit class EdgeIdToInt(x: EdgeId) { @inline def toInt: Int = x.asInstanceOf[Int] }
  implicit class NodeIdToInt(x: NodeId) { @inline def toInt: Int = x.asInstanceOf[Int] }
  implicit class IntToEdgeId(x: Int) { @inline def toEdgeId: EdgeId = x.asInstanceOf[EdgeId] }
  implicit class IntToNodeId(x: Int) { @inline def toNodeId: NodeId = x.asInstanceOf[NodeId] }

  type Triple = (Iterable[Edge], Node, Iterable[Edge]) // outputs, node, inputs

  var curEdgeId : EdgeId = 0.toEdgeId
  var curNodeId : NodeId = 0.toNodeId
  var curInputId: EdgeId = 0.toEdgeId

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
    val outputs = mutable.ArrayBuffer[EdgeId](0.toEdgeId)             // nodeId :: nodeId+1 ==> edges for this node
    val inputs  = mutable.ArrayBuffer[Seq[EdgeId]]()                 // Dataflow edges (reverse) -- per node
    val bounds  = mutable.ArrayBuffer[Seq[EdgeId]]()                 // Edges bound by this node
    val tunnels = mutable.ArrayBuffer[Seq[(EdgeId,Seq[EdgeId])]]()   // Edges bound by this node, defined elsewhere
    val freqs   = mutable.ArrayBuffer[Seq[Float]]()                  // Frequency hints for code motion
  }

  override def reset(): Unit = {
    super.reset()
    curEdgeId = 0.toEdgeId
    curNodeId = 0.toNodeId
    curInputId = 0.toEdgeId
    EdgeData.value.clear()
    EdgeData.producer.clear()
    EdgeData.dependents.clear()
    NodeData.value.clear()
    NodeData.outputs.clear()
    NodeData.outputs += 0.toEdgeId
    NodeData.inputs.clear()
    NodeData.bounds.clear()
    NodeData.tunnels.clear()
    NodeData.freqs.clear()
  }

  def nodeOutputs(node: NodeId): Seq[EdgeId] = {
    (NodeData.outputs(node.toInt).toInt until NodeData.outputs(node.toInt+1).toInt).map(_.toEdgeId)
  }
  def nodeInputs(node: NodeId): Seq[EdgeId] = NodeData.inputs(node.toInt)
  def nodeBounds(node: NodeId): Seq[EdgeId] = NodeData.bounds(node.toInt)
  def nodeTunnels(node: NodeId): Seq[(EdgeId,Seq[EdgeId])] = NodeData.tunnels(node.toInt)
  def nodeFreqs(node: NodeId): Seq[Float] = NodeData.freqs(node.toInt)
  def nodeOf(node: NodeId): Node = NodeData.value(node.toInt)
  def edgeOf(edge: EdgeId): Edge = EdgeData.value(edge.toInt)
  def dependentsOf(edge: EdgeId): Seq[NodeId] = EdgeData.dependents(edge.toInt)
  def producerOf(edge: EdgeId): NodeId = EdgeData.producer(edge.toInt)
  def triple(id: NodeId): Triple = (nodeOutputs(id).map(edgeOf), nodeOf(id), nodeInputs(id).map(edgeOf))

  def removeEdge(in: EdgeLike): Unit = {
    EdgeData.dependents(in._id) = Nil
  }

    /** Add an edge with no node or scheduling dependencies **/
  final def registerInput(in: EdgeLike): Unit = {
    in.id_=(curInputId.toInt)
    curInputId = (curInputId.toInt + 1).toEdgeId
  }

  /** Add an edge with no node but which may be required for scheduling **/
  final def addBound(out: Edge) = addNode(Nil, Seq(out), Nil, Nil, Nil, null).head

  /** Add output edge(s) with corresponding node **/
  final def addNode(ins: Seq[Edge], outs: Seq[Edge], binds: Seq[Edge], tunnel: Seq[(Edge,Seq[Edge])], freqs: Seq[Float], node: Node) = {
    outs.foreach { out =>
      out.id = curEdgeId.toInt

      EdgeData.value += out
      EdgeData.producer += curNodeId
      EdgeData.dependents += Nil

      curEdgeId = (curEdgeId.toInt + 1).toEdgeId
      out
    }
    ins.foreach { in => EdgeData.dependents(in.id) ::= curNodeId }

    if (node != null) node.id = curNodeId.toInt
    NodeData.value += node
    NodeData.outputs += curEdgeId
    NodeData.inputs += ins.map(_.id.toEdgeId)
    NodeData.bounds += binds.map(_.id.toEdgeId)
    NodeData.tunnels += tunnel.map{case (tun,results) => (tun.id.toEdgeId, results.map(_.id.toEdgeId)) }
    NodeData.freqs += freqs
    curNodeId = (curNodeId.toInt + 1).toNodeId

    outs
  }

  // --- Scheduling
  // Based on performance testing LongMap is slightly faster than others (even with casting)
  type OrderCache = scala.collection.mutable.LongMap[(NodeId,Int)] // EdgeId -> (NodeId,Int)
  def OrderCache() = new scala.collection.mutable.LongMap[(NodeId,Int)]()

  type SMap  = scala.collection.mutable.LongMap[Int] //java.util.HashMap[NodeId,Int]
  def SMap() = new mutable.LongMap[Int]() //new java.util.HashMap[NodeId,Int]()
  type Stack = java.util.ArrayDeque[NodeId]
  def Stack() = new java.util.ArrayDeque[NodeId]()


  private val comparator = new java.util.Comparator[(NodeId,Int)] {
    def compare(a:(NodeId,Int), b:(NodeId,Int)) = if (b._2 < a._2) -1 else if (b._2 == a._2) 0 else 1
  }

  //remember the original order of the nodes
  def buildScopeIndex(scope: Iterable[NodeId]): OrderCache = {
    val cache = OrderCache()
    var idx = 0
    for (node <- scope) {
      nodeOutputs(node).foreach{edge => cache.put(edge.toInt, (node,idx)) }
      idx += 1
    }
    cache
  }

  //performance hotspot!
  //should be O(1) wrt 'scope' (nodes in graph), try to keep this as efficient as possible
  def scheduleDepsWithIndex(roots: Iterable[EdgeId], cache: OrderCache): Seq[NodeId] = {
    val sortedSet = new java.util.TreeSet[(NodeId,Int)](comparator)

    for (edge <- roots) {
      cache.get(edge.toInt).foreach{node => sortedSet.add(node) }
    }

    var res: List[NodeId] = Nil
    val iter = sortedSet.iterator //return nodes in the original order given by 'scope'
    while (iter.hasNext) {
      res ::= iter.next._1
    }
    res
  }

  /** Linearize graph using DFS - assumes graph is a DAG **/
  def dfs(start: Iterable[NodeId], verbose: Boolean = false)(succ: NodeId => Iterable[NodeId]): List[NodeId] = {
    val visit = mutable.HashSet[NodeId]()
    val res = mutable.ListBuffer[NodeId]()
    start.foreach{node => traverse(node) }

    def traverse(node: NodeId) {
      if (!visit.contains(node)) {
        //if (verbose) log(c"  [DFS] ${triple(node)}")

        visit += node
        succ(node).foreach{node => traverse(node) }
        res += node
      }
    }

    res.result
  }

  /**
    * Tarjan's algorithm (linear).
    * Returns the strongly connected components of the graph rooted at the first argument,
    * whose edges are given by the function argument.
    *
    * The scc are returned in topological order.
    */
  def sccs(start: Iterable[NodeId])(succ: NodeId => Iterable[NodeId]): List[List[NodeId]] = {
    var id: Int = 0
    val stack = Stack()
    val mark = SMap()

    var res = List[List[NodeId]]()
    for (node <- start) { visit(node) }

    def visit(node: NodeId): Int = {
      if (mark.contains(node.toInt)) mark(node.toInt)
      else {
        id += 1
        mark.put(node.toInt, id)
        stack.addFirst(node)
        val min = succ(node).foldLeft(id){(m, node) => Math.min(m, visit(node)) }

        if (min == mark(node.toInt)) {
          var scc: List[NodeId] = Nil
          var element: NodeId = null.asInstanceOf[NodeId]
          do {
            element = stack.removeFirst()
            scc ::= element
            mark.put(element.toInt, Integer.MAX_VALUE)
          } while (element != node)
          res ::= scc
        }
        min
      }
    }
    res
  }



  // --- Visit methods (scoped)
  def ordered(node: NodeId, cache: OrderCache): Iterable[NodeId] = scheduleDepsWithIndex(nodeInputs(node), cache)
  def reverse(node: NodeId, scope: Set[NodeId]): Iterable[NodeId] = nodeInputs(node).map(producerOf) filter scope.contains
  def forward(node: NodeId, scope: Set[NodeId]): Iterable[NodeId] = nodeOutputs(node).flatMap(dependentsOf) filter scope.contains
  def noCold(node: NodeId, scope: Set[NodeId]): Iterable[NodeId] = {
    nodeInputs(node).zip(nodeFreqs(node)).filter(_._2 > 0.75f).map(x => producerOf(x._1)) filter scope.contains
  }
  def noHot(node: NodeId, scope: Set[NodeId]): Iterable[NodeId] = {
    nodeInputs(node).zip(nodeFreqs(node)).filter(_._2 < 100f).map(x => producerOf(x._1)) filter scope.contains
  }


  // --- Visit methods (unscoped)
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
    def getDependents(root: NodeId) = {
      val rootEdges = nodeOutputs(root)

      // Get dependents of this node, stopping at nodes which bind the root
      def uses(node: NodeId) = forward(node,scope) filterNot {next => nodeBounds(next) exists (rootEdges contains _) }

      //log(c"    [root = ${triple(root)}]")

      dfs(List(root))(uses)
    }

    val boundDependents = scope.flatMap{node =>
      val bounds = nodeBounds(node).map(producerOf)
      val sched = bounds.flatMap(getDependents)

//      if (bounds.nonEmpty) {
//        log(c"  [Bounds] node: ${triple(node)}")
//        log(c"  [Bounds] bounds:")
//        bounds.foreach{bnd => log(c"  [Bounds]  ${triple(bnd)}")}
//        log(c"  [Bounds] binded: ")
//        sched.foreach{s => log(c"  [Bounds]  ${triple(s)}") }
//      }
      sched
    }

    val tunnelDependents = scope.flatMap{node =>
//      if (nodeTunnels(node).nonEmpty)
//        log(c"  [Tunnel] node: ${triple(node)}: ")

      nodeTunnels(node).flatMap{case (tun,res) =>
        val tunnel = producerOf(tun)
        // Everything upstream of (prior to) the block results for this tunnel
        val results = res.map(producerOf)
        // Everything downstream of (depending on) the tunnel
        val schedule = dfs(results){node => reverse(node,scope) }
        val forward  = getDependents(tunnel) filterNot (_ == tunnel)

//        log(c"  [Tunnel] tunnel: ")
//        log(c"  [Tunnel]  ${triple(tunnel)}")
//        log(c"  [Tunnel] schedule: ")
//        schedule.foreach{stm => log(c"  [Tunnel]  ${triple(stm)}")}
//        log(c"  [Tunnel] dependents: ")
//        forward.foreach{stm => log(c"  [Tunnel]  ${triple(stm)}")}

        schedule intersect forward
      }
    }
    boundDependents ++ tunnelDependents
  }

  /**
    * ISSUE #5: Major difference between LMS and Argon
    *
    * In both LMS and Argon, a Def containing a Block has a dependency on the block's result
    * If the block's result is a Reify node in LMS, it will have anti-dependencies on ALL effects in the block
    * However, if we are scheduling the Def with that block, these anti-dependencies should never be in the local scope
    * Instead, they should be delayed until traversing the block itself (since they are bound to that block)
    * In Argon, those anti-dependencies are properties of the block, not of the result symbol.
    * This means we will never even attempt to schedule anything but dataflow order from the result
    * until we actually are traversing that block.
    * The one drawback of this is that we cannot code motion non-effectful dependencies of block effects
    */

  def getLocalScope(currentScope: Seq[NodeId], result: Seq[EdgeId], localCache: OrderCache): Seq[NodeId] = {
    val scope = currentScope.toSet

    // TODO: This was the entire focused scope in LMS (descriptively called "e1")
    // But that appears to cause things to leak out of cold blocks...
    val roots = result.map(producerOf) //scheduleDepsWithIndex(result, localCache)

    val mustInside = getBoundDependents(scope)

    val mayOutside = scope diff mustInside
    val fringe = mustInside.flatMap(reverse) intersect mayOutside

    // 2. Statements that can be reached without following any cold/hot inputs, respectively
    // log("Computing reachableWarm: ")
    val reachableWarm = dfs(roots)(node => noCold(node,scope)).toSet
    // log("Computing reachableCold: ")
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

// These log statements are VERY expensive - use only when debugging
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


  def getLocalSchedule[A](availableNodes: Seq[NodeId], result: Seq[EdgeId]): Seq[NodeId] = {
    val availCache = buildScopeIndex(availableNodes)
    val availRoots = scheduleDepsWithIndex(result, availCache)
    val localNodes = getSchedule(availRoots, availCache, checkAcyclic = true)

    val localCache = buildScopeIndex(localNodes) // NOTE: Can't use localScope here (as it may not be fully ordered)
    val localScope = getLocalScope(localNodes, result, localCache)
    val localRoots = scheduleDepsWithIndex(result, localCache)
    val localSchedule = getSchedule(localRoots, localCache, checkAcyclic = false) filter (localScope contains _)

// These log statements are VERY expensive - use only when debugging
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
