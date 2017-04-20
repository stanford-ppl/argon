package argon.codegen.dotgen

import argon.core.Staging
import argon.ops.VoidExp

trait DotGenVoid extends DotCodegen {
  val IR: Staging with VoidExp
  import IR._

}
