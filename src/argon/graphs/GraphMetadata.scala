package argon.graphs

import scala.collection.mutable

trait GraphMetadata[EdgeId,Metadata] {
  def keyOf[M<:Metadata:Manifest] = manifest[M].runtimeClass.asInstanceOf[Class[M]]

  /**
    * Maintained as an Array of Maps. An Array of Sets nearly works, but requires either making the equals method
    * on metadata instances otherwise useless, or spending linear time searching for existing entries with the same key
    */
  val globalMetadata = mutable.ArrayBuffer[Map[Class[_],Metadata]]()

  def getMetadata(edge: EdgeId): Map[Class[_],Metadata]
  def addMetadata(edge: EdgeId, m: Metadata): Unit
  def setMetadata(edge: EdgeId, m: Map[Class[_],Metadata]): Unit
  def removeMetadata(edge: EdgeId, m: Metadata): Unit
}
