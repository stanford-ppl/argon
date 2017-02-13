package argon.codegen.scalagen

import argon.codegen.Codegen
import sys.process._
import scala.language.postfixOps
import argon.codegen.FileDependencies

trait ScalaCodegen extends Codegen with FileDependencies{
	
  import IR._
  override val name = "Scala Codegen"
  override val lang: String = "scala"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"${b.result}")
  }

  override def copyDependencies(out: String): Unit = {
    // FIXME: Should be OS-independent. Ideally want something that also supports wildcards, maybe recursive copy
    s"""cp ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/scalagen/resources/Makefile ${out}/..""".!
    s"""cp ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/run.sh ${out}/..""".!
    super.copyDependencies(out)
  }

}
