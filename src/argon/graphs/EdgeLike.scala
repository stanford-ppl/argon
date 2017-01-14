package argon.graphs

trait EdgeLike {
  private[graphs] var _id: Int = 0
  private[graphs] def id_=(i: Int) { _id = i }
}
