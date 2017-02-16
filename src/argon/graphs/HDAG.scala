 package argon.graphs

import scala.collection.mutable

trait HDAG extends AbstractHDAG with GraphMetadata {
  override type EdgeId = Int
  override type NodeId = Int
}
