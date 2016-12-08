package argon.graphs

import scala.collection.mutable

trait HDAG extends AbstractHDAG with GraphMetadata {
  override type EdgeId = Int
  override type NodeId = Int

  // Based on performance testing LongMap is slightly faster than others (even with casting)
  type OrderCache = scala.collection.mutable.LongMap[(NodeId,Int)]
  def OrderCache() = new scala.collection.mutable.LongMap[(NodeId,Int)]()

  private val comparator = new java.util.Comparator[(NodeId,Int)] {
    def compare(a:(NodeId,Int), b:(NodeId,Int)) = if (b._2 < a._2) -1 else if (b._2 == a._2) 0 else 1
  }

  //remember the original order of the nodes
  override def buildScopeIndex(scope: Iterable[NodeId]): OrderCache = {
    val cache = OrderCache()
    var idx = 0
    for (node <- scope) {
      nodeOutputs(node).foreach{edge => cache.put(edge, (node,idx)) }
      idx += 1
    }
    cache
  }

  //performance hotspot!
  //should be O(1) wrt 'scope' (nodes in graph), try to keep this as efficient as possible
  override def scheduleDepsWithIndex(roots: Iterable[EdgeId], cache: OrderCache): List[NodeId] = {
    val sortedSet = new java.util.TreeSet[(NodeId,Int)](comparator)

    for (edge <- roots) {
      cache.get(edge).foreach{node => sortedSet.add(node) }
    }

    var res: List[NodeId] = Nil
    val iter = sortedSet.iterator //return nodes in the original order given by 'scope'
    while (iter.hasNext) {
      res ::= iter.next._1
    }
    res
  }

  type SMap  = scala.collection.mutable.LongMap[Int] //java.util.HashMap[NodeId,Int]
  def SMap() = new mutable.LongMap[Int]() //new java.util.HashMap[NodeId,Int]()
  type Stack = java.util.ArrayDeque[NodeId]
  def Stack() = new java.util.ArrayDeque[NodeId]()

  /** Linearize graph using DFS - assumes graph is a DAG **/
  def dfs(start: Iterable[NodeId])(succ: NodeId => Iterable[NodeId]): List[NodeId] = {
    val visit = mutable.HashSet[NodeId]()
    val res = mutable.ListBuffer[NodeId]()
    start.foreach{node => traverse(node) }

    def traverse(node: NodeId) {
      if (!visit.contains(node)) {
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
      if (mark.contains(node)) mark(node)
      else {
        id += 1
        mark.put(node, id)
        stack.addFirst(node)
        val min = succ(node).foldLeft(id){(m, node) => Math.min(m, visit(node)) }

        if (min == mark(node)) {
          var scc: List[NodeId] = Nil
          var element: NodeId = null.asInstanceOf[NodeId]
          do {
            element = stack.removeFirst()
            scc ::= element
            mark.put(element, Integer.MAX_VALUE)
          } while (element != node)
          res ::= scc
        }
        min
      }
    }
    res
  }
}
