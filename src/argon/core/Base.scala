package argon.core

import argon.State
import scala.virtualized.{SourceContext,EmbeddedControls}

trait Base extends EmbeddedControls with Reporting {
  type SrcCtx = SourceContext
  type Staged[T]

  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}
