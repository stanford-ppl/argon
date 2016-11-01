package argon.core

import argon.graphs.GraphMetadata

trait Metadata extends Lattices { self: Statements =>

  val graph: GraphMetadata[Int,Metadata[_]]

  abstract class Metadata[T] { self =>
    def meet(that: T): T = this.asInstanceOf[T]
    def join(that: T): T = this.asInstanceOf[T]
    def isEmpiric: Boolean = true
    def mirror(f: Tx): T
    final def key = self.getClass
    override final def hashCode(): Int = key.hashCode()
  }

  implicit def metadataHasLattice[T <: Metadata[T]]: Lattice[T] = new Lattice[T] {
    override def meet(a: T, b: T): T = a.meet(b)
    override def join(a: T, b: T): T = a.join(b)
    override def isEmpiric(a: T): Boolean = a.isEmpiric
    val top = None
    val bottom = None
  }

  object metadata {
    private def keyOf[M<:Metadata[M]:Manifest] = manifest[M].runtimeClass.asInstanceOf[Class[M]]

    def add[M<:Metadata[M]:Manifest](edge:Sym, m:M):Unit = this.add(edge.id, Some(m))
    def add[M<:Metadata[M]:Manifest](edge:Sym, m:Option[M]):Unit = this.add(edge.id, m)
    private def add[M<:Metadata[M]:Manifest](edge:Int, m:M):Unit = this.add(edge, Some(m))
    private def add[M<:Metadata[M]:Manifest](edge:Int, m:Option[M]):Unit = {
      val meta = graph.getMetadata(edge)
      val k = keyOf[M]
      val prev = meta.find(_.key == k).map(_.asInstanceOf[M])
      val entry = join(m, prev) //metaUpdate(m, prev)
      if (entry.isDefined) graph.addMetadata(edge, entry.get)
      else if (prev.isDefined) graph.removeMetadata(edge, prev.get)
    }
    def apply[M<:Metadata[M]:Manifest](edge:Sym):Option[M] = this.apply[M](edge.id)
    private def apply[M<:Metadata[M]:Manifest](edge:Int):Option[M] = {
      val k = keyOf[M]
      graph.getMetadata(edge).find(_.key == k).map(_.asInstanceOf[M])
    }
    def get(edge:Sym):Set[Metadata[_]] = graph.getMetadata(edge.id)
    def set(edge:Sym, m:Set[Metadata[_]]):Unit = graph.setMetadata(edge.id, m)
  }
}
