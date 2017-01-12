package argon.codegen

import sys.process._
import scala.language.postfixOps

trait FileDependencies extends Codegen {
  import IR._

  sealed abstract class CodegenDep {
    def input: String
    def outputPath: String
    def needsCopy: Boolean
  }

  case class AlwaysDep(input: String, outputPath: String = "") extends CodegenDep { def needsCopy = true }

  case class CondDep(input: String, outputPath: String = "", add: () => Boolean) extends CodegenDep { def needsCopy = add() }

  var dependencies: List[CodegenDep] = Nil

  // FIXME: Should be OS-independent. Ideally want something that also supports wildcards, maybe recursive copy
  def copyDependencies(out: String): Unit = {
    dependencies.foreach{dep => if (dep.needsCopy) {
      log(s"Copying ${dep.input} to $out")
      s"cp ${dep.input} ${out}${java.io.File.separator}${dep.outputPath}" !
    }}
  }
  override def postprocess[S:Staged](b:Scope[S]) = {
    copyDependencies(out)
    super.postprocess(b)
  }
}
