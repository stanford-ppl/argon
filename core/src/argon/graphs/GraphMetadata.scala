package argon.graphs

import scala.collection.mutable

class GraphMetadata[MetaData] {
  def keyOf[M <: MetaData: Manifest] =
    manifest[M].runtimeClass.asInstanceOf[Class[M]]

  /**
    * Maintained as an Array of Maps. An Array of Sets nearly works, but requires either making the equals method
    * on metadata instances otherwise useless, or spending linear time searching for existing entries with the same key
    */
  val edgeMetadata  = mutable.ArrayBuffer[Map[Class[_], MetaData]]()
  val otherMetadata = mutable.ArrayBuffer[Map[Class[_], MetaData]]()

  final def getMetadata(edge: EdgeLike): Map[Class[_], MetaData] = edge match {
    case e: Edge =>
      if (e.id < edgeMetadata.length) edgeMetadata(e.id) else Map.empty
    case e: EdgeLike =>
      if (e._id < otherMetadata.length) otherMetadata(e._id) else Map.empty
  }

  private def add(
      id: Int,
      m: MetaData,
      metadata: mutable.ArrayBuffer[Map[Class[_], MetaData]]): Unit = {
    while (id >= metadata.length) metadata += Map.empty[Class[_], MetaData]

    metadata(id) += (m.getClass -> m)
  }

  final def addMetadata(edge: EdgeLike, m: MetaData): Unit = edge match {
    case e: Edge     => add(e.id, m, edgeMetadata)
    case e: EdgeLike => add(e._id, m, otherMetadata)
  }
  final def setMetadata(edge: EdgeLike, m: Map[Class[_], MetaData]): Unit =
    edge match {
      case e: Edge     => edgeMetadata(e.id) = m
      case e: EdgeLike => otherMetadata(e._id) = m
    }
  final def removeMetadata(edge: EdgeLike, m: MetaData): Unit = edge match {
    case e: Edge     => edgeMetadata(e.id) -= m.getClass
    case e: EdgeLike => otherMetadata(e._id) -= m.getClass
  }

  final def clearMetadata(m: Class[_]): Unit = {
    edgeMetadata.indices.foreach { id =>
      edgeMetadata(id) -= m
    }
    otherMetadata.indices.foreach { id =>
      otherMetadata(id) -= m
    }
  }

  def reset(): Unit = {
    edgeMetadata.clear()
    otherMetadata.clear()
  }
}
