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
  var moveDependencies: List[CodegenDep] = Nil
  var patchDependencies: List[CodegenDep] = Nil

  // FIXME: Should be OS-independent. Ideally want something that also supports wildcards, maybe recursive copy
  def copyDependencies(out: String): Unit = {
    // Files that need to cp
    dependencies.foreach{dep => if (dep.needsCopy) {
      s"mkdir -p ${out}${java.io.File.separator}${dep.outputPath}" !
    }}
    dependencies.foreach{dep => if (dep.needsCopy) {
      log(s"cp -r ${dep.input} ${out}${java.io.File.separator}${dep.outputPath}")
      s"cp -r ${dep.input} ${out}${java.io.File.separator}${dep.outputPath}" !
    }}
    // Files that need to mv
    moveDependencies.foreach{dep => if (dep.needsCopy) {
      s"mkdir -p ${out}${java.io.File.separator}${dep.outputPath}" !
    }}
    moveDependencies.foreach{dep => if (dep.needsCopy) {
      log(s"mv ${dep.input} ${out}${java.io.File.separator}${dep.outputPath}")
      s"mv ${dep.input} ${out}${java.io.File.separator}${dep.outputPath}" !
    }}
    patchDependencies.foreach{dep => if (dep.needsCopy) {
      s"${dep.input}" !
    }}
  }
  override protected def postprocess[S:Type](b: Block[S]) = {
    copyDependencies(out)
    super.postprocess(b)
  }
}
