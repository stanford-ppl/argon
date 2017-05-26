package argon.core

/**
  * Non-threadsafe compiler state.
  * If possible, use State instead.
  */
object Globals {
  /** Compiler is doing initial staging of user code **/
  var staging: Boolean = false
}
