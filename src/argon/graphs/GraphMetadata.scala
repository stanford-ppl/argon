package argon.graphs

import scala.collection.mutable

trait GraphMetadata[EdgeId,Metadata] {
  def keyOf[M<:Metadata:Manifest] = manifest[M].runtimeClass.asInstanceOf[Class[M]]

  val globalMetadata = mutable.ArrayBuffer[Set[Metadata]]()

  def getMetadata(edge: EdgeId): Set[Metadata]
  def addMetadata(edge: EdgeId, m: Metadata): Unit
  def setMetadata(edge: EdgeId, m: Set[Metadata]): Unit
  def removeMetadata(edge: EdgeId, m: Metadata): Unit
}
