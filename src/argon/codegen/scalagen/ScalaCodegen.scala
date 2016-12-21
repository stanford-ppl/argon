package argon.codegen.scalagen

import argon.codegen.Codegen

trait ScalaCodegen extends Codegen {
  import IR._

  override val lang: String = "scala"
  override val ext: String = "scala"
}
