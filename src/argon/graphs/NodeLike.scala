package argon.graphs

import scala.language.implicitConversions

trait NodeLike[Id,Node] {
  def setId(node: Node, id: Id): Unit
  def getId(node: Node): Id

  class Ops(node: Node) {
    def id_=(id: Id): Unit = setId(node, id)
    def id: Id = getId(node)
  }
  implicit def nodeToOps(node: Node): Ops = new Ops(node)
}
