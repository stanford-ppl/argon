package argon.codegen.dotgen

import argon.ops.VoidExp

trait DotGenVoid extends DotCodegen {
  val IR: VoidExp
  import IR._

}
