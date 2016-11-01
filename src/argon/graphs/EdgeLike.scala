package argon.graphs

import scala.language.implicitConversions

trait EdgeLike[Id,Edge] {
  def setId(edge: Edge, id: Id): Unit
  def getId(edge: Edge): Id

  class Ops(edge: Edge) {
    def id_=(id: Id): Unit = setId(edge, id)
    def id: Id = getId(edge)
  }
  implicit def edgeToOps(edge: Edge): Ops = new Ops(edge)
}
