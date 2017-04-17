package argon

object State {
  var staging: Boolean = false
  var flex: Boolean    = false
  var EVAL: Boolean    = false

  def inFlex[T](x: => T): T = {
    val save = State.flex
    State.flex = true
    val result = x
    State.flex = save
    result
  }
  def inLib[T](x: => T): T = {
    val save = State.EVAL
    State.EVAL = true
    val result = inFlex { x }
    State.EVAL = save
    result
  }

  var pass: Int = 1

  def paddedPass: String = {
    val p = pass.toString
    "0" * (4 - p.length) + p
  }
}
