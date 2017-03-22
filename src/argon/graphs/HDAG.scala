package argon.graphs

trait HDAG extends AbstractHDAG with GraphMetadata {
  override type EdgeId = Int
  override type NodeId = Int

  override def reset(): Unit = {
    super.reset()
    edgeMetadata.clear()
    otherMetadata.clear()
  }
}
