package argon.core

import argon.State

trait Base {
  type SrcCtx = virtualized.SourceContext

  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }

}
