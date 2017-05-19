package argon.core
package cake

import forge._

trait MetadataLayer { this: ArgonCore =>

  implicit def metadataHasLattice[T <: Metadata[T]]: Lattice[T] = new Lattice[T] {
    override def meet(a: T, b: T): T = a.meet(b)
    override def join(a: T, b: T): T = a.join(b)
    override def isEmpiric(a: T): Boolean = a.isEmpiric
    val top = None
    val bottom = None
  }

  /** Shortcuts for metadata **/
  object metadata {
    @stateful def apply[M<:Metadata[M]:Manifest](edge: Exp[_])(implicit state: State): Option[M] = state.metadata[M](edge)
    @stateful def add[M<:Metadata[M]:Manifest](edge: Exp[_], m: M)(implicit state: State): Unit = state.metadata.add[M](edge, m)
    @stateful def get(edge: Exp[_])(implicit state: State): Map[Class[_],Metadata[_]] = state.metadata.get(edge)
  }

}
