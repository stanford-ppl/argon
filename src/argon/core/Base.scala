package argon.core

import argon.State
import virtualized.{Typs,EmbeddedControls}

trait Base extends Typs with EmbeddedControls {
  type SrcCtx = virtualized.SourceContext

  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}
