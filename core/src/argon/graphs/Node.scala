package argon.graphs

trait Node {
  private var _id: Int = 0
  private[graphs] def id_=(i: Int) { _id = i }
  def id: Int = _id
}
