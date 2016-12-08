package argon.core

import argon.graphs.{HDAG,Edge}

trait Metadata extends HDAG with Lattices { self: Statements =>
  type MetaData = Metadata[_]

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

    def add[M<:Metadata[M]:Manifest](edge:Edge, m:M):Unit = this.add(edge.id, Some(m))
    def add[M<:Metadata[M]:Manifest](edge:Edge, m:Option[M]):Unit = this.add(edge.id, m)
    private def add[M<:Metadata[M]:Manifest](id:Int, m:M):Unit = this.add(id, Some(m))
    private def add[M<:Metadata[M]:Manifest](id:Int, m:Option[M]):Unit = {
      val meta = getMetadata(id)
      val k = keyOf[M]
      val prev = meta.get(k).map(_.asInstanceOf[M])
      val entry = join(m, prev) //metaUpdate(m, prev)
      if (entry.isDefined) addMetadata(id, entry.get)
      else if (prev.isDefined) removeMetadata(id, prev.get)
    }
    def apply[M<:Metadata[M]:Manifest](edge: Edge): Option[M] = this.apply[M](edge.id)
    private[Metadata] def apply[M<:Metadata[M]:Manifest](id:Int):Option[M] = {
      val k = keyOf[M]
      getMetadata(id).get(k).map(_.asInstanceOf[M])
    }
    def get(edge:Edge):Map[Class[_],Metadata[_]] = getMetadata(edge.id)
    def set(edge:Edge, m:Map[Class[_],Metadata[_]]):Unit = setMetadata(edge.id, m)
  }
}
