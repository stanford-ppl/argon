package argon.graphs

import scala.collection.mutable

trait GraphMetadata {
  type MetaData
  type EdgeId = Int
  def keyOf[M<:MetaData:Manifest] = manifest[M].runtimeClass.asInstanceOf[Class[M]]

  /**
    * Maintained as an Array of Maps. An Array of Sets nearly works, but requires either making the equals method
    * on metadata instances otherwise useless, or spending linear time searching for existing entries with the same key
    */
  val globalMetadata = mutable.ArrayBuffer[Map[Class[_],MetaData]]()

  def getMetadata(edge: EdgeId): Map[Class[_],MetaData] = {
    if (edge < globalMetadata.length) globalMetadata(edge) else Map.empty
  }
  def addMetadata(edge: EdgeId, m: MetaData): Unit = {
    while (edge >= globalMetadata.length)
      globalMetadata += Map.empty[Class[_],MetaData]

    globalMetadata(edge) += (m.getClass -> m)
  }
  def setMetadata(edge: EdgeId, m: Map[Class[_],MetaData]): Unit = globalMetadata(edge) = m
  def removeMetadata(edge: EdgeId, m: MetaData): Unit = globalMetadata(edge) -= m.getClass

}
