package argon.core

import argon._
import forge._

trait MetadataCore { this: ArgonCore =>

  implicit def metadataHasLattice[T <: Metadata[T]]: Lattice[T] = new Lattice[T] {
    override def meet(a: T, b: T): T = a.meet(b)
    override def join(a: T, b: T): T = a.join(b)
    override def isEmpiric(a: T): Boolean = a.isEmpiric
    val top = None
    val bottom = None
  }

  /** Shortcuts for metadata **/
  object metadata {
    @stateful def apply[M<:Metadata[M]:Manifest](edge: Exp[_]): Option[M] = state.metadata[M](edge)
    @stateful def add[M<:Metadata[M]:Manifest](edge: Exp[_], m: M): Unit = state.metadata.add[M](edge, m)
    @stateful def get(edge: Exp[_]): Map[Class[_],Metadata[_]] = state.metadata.get(edge)
  }

}
