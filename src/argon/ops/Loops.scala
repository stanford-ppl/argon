package argon.ops

import argon.core.Staging

trait LoopOps
trait LoopApi extends LoopOps
trait LoopExp extends LoopOps with Staging {

  abstract class AbstractLoop[T:Staged] extends Op[T]

}
