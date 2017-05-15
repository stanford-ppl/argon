package argon.graphs

import java.io.PrintStream

import argon._
import argon.core._

import scala.collection.mutable

class Graph[E<:Edge,N<:Node] {
  type EdgeId = Int
  type NodeId = Int
  final val VERBOSE_SCHEDULING = false
  var glog: PrintStream = if (VERBOSE_SCHEDULING) createLog(Config.logDir + "/sched/", "0000 Staging.log") else null
  @inline private def xlog(x: String) = if (VERBOSE_SCHEDULING) withLog(glog){ log(x) }

  implicit class EdgeIdToInt(x: EdgeId) { @inline def toInt: Int = x.asInstanceOf[Int] }
  implicit class NodeIdToInt(x: NodeId) { @inline def toInt: Int = x.asInstanceOf[Int] }
  implicit class IntToEdgeId(x: Int) { @inline def toEdgeId: EdgeId = x.asInstanceOf[EdgeId] }
  implicit class IntToNodeId(x: Int) { @inline def toNodeId: NodeId = x.asInstanceOf[NodeId] }

  type Triple = (Iterable[E], N, Iterable[E]) // outputs, node, inputs

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
    val value = mutable.ArrayBuffer[E]()
    val producer = mutable.ArrayBuffer[NodeId]()            // Index of node corresponding to this edge
    val dependents = mutable.ArrayBuffer[List[NodeId]]()    // Dataflow edges (forward) -- per edge
  }

  object NodeData {
    val value   = mutable.ArrayBuffer[N]()
    val outputs = mutable.ArrayBuffer[EdgeId](0.toEdgeId)            // nodeId :: nodeId+1 ==> edges for this node
    val inputs  = mutable.ArrayBuffer[Seq[EdgeId]]()                 // Dataflow edges (reverse) -- per node
    val bounds  = mutable.ArrayBuffer[Seq[EdgeId]]()                 // Edges bound by this node
    val tunnels = mutable.ArrayBuffer[Seq[(EdgeId,Seq[EdgeId])]]()   // Edges bound by this node, defined elsewhere
    val freqs   = mutable.ArrayBuffer[Seq[Freq]]()                   // Frequency hints for code motion
  }

  def reset(): Unit = {
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
  def nodeFreqs(node: NodeId): Seq[Freq] = NodeData.freqs(node.toInt)
  def nodeOf(node: NodeId): N = NodeData.value(node.toInt)
  def edgeOf(edge: EdgeId): E = EdgeData.value(edge.toInt)
  def dependentsOf(edge: EdgeId): Seq[NodeId] = EdgeData.dependents(edge.toInt)
  def producerOf(edge: EdgeId): NodeId = EdgeData.producer(edge.toInt)
  def triple(id: NodeId): Triple = (nodeOutputs(id).map(edgeOf), nodeOf(id), nodeInputs(id).map(edgeOf))

  def removeEdge(in: EdgeLike): Unit = {
    // TODO: What is the safe/sensible thing to do here?
    //EdgeData.dependents(in._id) = Nil
    xlog(c"REMOVING EDGE $in")
  }

    /** Add an edge with no node or scheduling dependencies **/
  final def registerInput(in: EdgeLike): Unit = {
    in.id_=(curInputId.toInt)
    curInputId = (curInputId.toInt + 1).toEdgeId
  }

  /** Add an edge with no node but which may be required for scheduling **/
  final def addBound(out: E) = addNode(Nil, Seq(out), Nil, Nil, Nil, null.asInstanceOf[N]).head

  /** Add output edge(s) with corresponding node **/
  final def addNode(ins: Seq[E], outs: Seq[E], binds: Seq[E], tunnel: Seq[(Edge,Seq[E])], freqs: Seq[Freq], node: N) = {
    outs.foreach { out =>
      out.id = curEdgeId.toInt

      EdgeData.value += out
      EdgeData.producer += curNodeId
      EdgeData.dependents += Nil

      curEdgeId = (curEdgeId.toInt + 1).toEdgeId
      out
    }

    xlog(c"Registering node $outs = $node")
    xlog(c"  Inputs: $ins")
    xlog(c"  Freqs:  $freqs")
    xlog(c"  Binds:  $binds")

    ins.foreach { in =>
      val deps = EdgeData.dependents(in.id)
      if (!deps.contains(curNodeId)) {
        EdgeData.dependents(in.id) = curNodeId +: deps
        xlog(c"  Adding dependency to input $in = ${nodeOf(producerOf(in.id))}")
        xlog(c"  Dependencies: ${EdgeData.dependents(in.id)}")
      }
    }

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
  def dfs(start: Iterable[NodeId])(succ: NodeId => Iterable[NodeId]): List[NodeId] = {
    val visit = mutable.HashSet[NodeId]()
    val res = mutable.ListBuffer[NodeId]()
    start.foreach{node => traverse(node) }

    def traverse(node: NodeId, tab: Int = 0) {
      if (!visit.contains(node)) {
        val successors = succ(node)
        xlog(c"[DFS]" + "  "*tab + s"${triple(node)} -> " + successors.flatMap(nodeOutputs).mkString(", "))

        visit += node
        successors.foreach{node => traverse(node, tab+1) }
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
    nodeInputs(node).zip(nodeFreqs(node)).filter(_._2 != Freq.Cold).map(x => producerOf(x._1)) filter scope.contains
  }
  def noHot(node: NodeId, scope: Set[NodeId]): Iterable[NodeId] = {
    nodeInputs(node).zip(nodeFreqs(node)).filter(_._2 != Freq.Hot).map(x => producerOf(x._1)) filter scope.contains
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

      // All nodes that depend on this node except nodes which bind the given root
      def uses(node: NodeId) = {
        if (VERBOSE_SCHEDULING) {
          forward(node, scope).foreach{next => xlog(c"[USES] ${triple(next)} (bounds = " + nodeBounds(next).mkString(",") + ")") }
        }

        forward(node,scope) filterNot {next => nodeBounds(next) exists (rootEdges contains _) }
      }

      dfs(List(root))(uses)
    }

    val boundDependents = scope.flatMap{node =>
      val bounds = nodeBounds(node).map(producerOf)
      val sched = bounds.flatMap(getDependents)

      if (VERBOSE_SCHEDULING) {
        if (bounds.nonEmpty) {
          xlog(c"  [Bounds] node: ${triple(node)}")
          xlog(c"  [Bounds] bounds:")
          bounds.foreach{bnd => xlog(c"  [Bounds]  ${triple(bnd)}")}
          xlog(c"  [Bounds] binded: ")
          sched.foreach{s => xlog(c"  [Bounds]  ${triple(s)}") }
        }
      }

      sched
    }

    val tunnelDependents = scope.flatMap{node =>
      if (VERBOSE_SCHEDULING) {
        if (nodeTunnels(node).nonEmpty) xlog(c"  [Tunnel] node: ${triple(node)}: ")
      }

      nodeTunnels(node).flatMap{case (tun,res) =>
        val tunnel = producerOf(tun)
        // Everything upstream of (prior to) the block results for this tunnel
        val results = res.map(producerOf)
        // Everything downstream of (depending on) the tunnel
        val schedule = dfs(results){node => reverse(node,scope) }
        val forward  = getDependents(tunnel) filterNot (_ == tunnel)

        if (VERBOSE_SCHEDULING) {
          xlog(c"  [Tunnel] tunnel: ")
          xlog(c"  [Tunnel]  ${triple(tunnel)}")
          xlog(c"  [Tunnel] schedule: ")
          schedule.foreach { stm => xlog(c"  [Tunnel]  ${triple(stm)}") }
          xlog(c"  [Tunnel] dependents: ")
          forward.foreach { stm => xlog(c"  [Tunnel]  ${triple(stm)}") }
        }

        schedule intersect forward
      }
    }
    boundDependents ++ tunnelDependents
  }


  def getLocalScope(currentScope: Seq[NodeId], result: Seq[EdgeId], localCache: OrderCache): Seq[NodeId] = {
    val scope = currentScope.toSet

    // TODO: This was the entire focused scope in LMS (descriptively called "e1")
    // But that appears to cause things to leak out of cold blocks...
    val roots = result.map(producerOf) //scheduleDepsWithIndex(result, localCache)

    val mustInside = getBoundDependents(scope)

    val mayOutside = scope diff mustInside
    val fringe = mustInside.flatMap(reverse) intersect mayOutside

    // 2. Statements that can be reached without following any cold/hot inputs, respectively
    // xlog("Computing reachableWarm: ")
    val reachableWarm = dfs(roots)(node => noCold(node,scope)).toSet
    // xlog("Computing reachableCold: ")
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

// These xlog statements are VERY expensive - use only when debugging
    if (VERBOSE_SCHEDULING) {
      xlog("Getting scope level within scope: ")
      scope.foreach{stm => xlog(c"  ${triple(stm)}"); xlog(c"    dependents = ${nodeOutputs(stm).flatMap(dependentsOf)}") }
      xlog("For result: ")
      result.foreach{s => xlog(c"  ${triple(producerOf(s))}") }
      xlog("Must inside: ")
      mustInside.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("May outside: ")
      mayOutside.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("Fringe: ")
      fringe.foreach{stm => xlog(c"  ${triple(stm)}") }
      xlog("Reachable [Warm]: ")
      reachableWarm.foreach{stm => xlog(c"  ${triple(stm)}") }
      xlog("Reachable [Cold]: ")
      reachableCold.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog(s"Hot paths: ")
      hotPathsOnly.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog(s"Hot dependencies: ")
      hotDeps.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog(s"Hot fringe: ")
      hotFringe.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("Hot fringe dependencies: ")
      hotFringeDeps.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("levelScope: ")
      levelScope.foreach{stm => xlog(c"  ${triple(stm)}")}
    }

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

// These xlog statements are VERY expensive - use only when debugging
    if (VERBOSE_SCHEDULING) {
      xlog("avail nodes: ")
      availableNodes.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("avail roots:")
      availRoots.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("local nodes:")
      localNodes.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("local scope: ")
      localScope.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("local roots:")
      localRoots.foreach{stm => xlog(c"  ${triple(stm)}")}
      xlog("local schedule:")
      localSchedule.foreach{stm => xlog(c"  ${triple(stm)}")}
    }

    localSchedule
  }
}
