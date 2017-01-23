package argon.core

import argon.graphs.HDAG

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

    def add[M<:Metadata[M]:Manifest](edge:Exp[_], m:M):Unit = this.add(edge, Some(m))
    def add[M<:Metadata[M]:Manifest](edge:Exp[_], m:Option[M]):Unit = {
      val meta = getMetadata(edge)
      val k = keyOf[M]
      val prev = meta.get(k).map(_.asInstanceOf[M])
      val entry = join(m, prev) //metaUpdate(m, prev)
      if (entry.isDefined) addMetadata(edge, entry.get)
      else if (prev.isDefined) removeMetadata(edge, prev.get)
    }
    def apply[M<:Metadata[M]:Manifest](edge: Exp[_]):Option[M] = {
      val k = keyOf[M]
      getMetadata(edge).get(k).map(_.asInstanceOf[M])
    }
    def get(edge:Exp[_]):Map[Class[_],Metadata[_]] = getMetadata(edge)
    def set(edge:Exp[_], m:Map[Class[_],Metadata[_]]):Unit = setMetadata(edge, m)

    def clearAll[M<:Metadata[M]:Manifest] = clearMetadata(keyOf[M])
  }
}
