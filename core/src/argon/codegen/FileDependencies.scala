package argon.codegen

import sys.process._
import scala.language.postfixOps
import org.apache.commons.io._
import java.io.File

trait FileDependencies extends Codegen {
  import IR._

  def copyFile(folder:String, in: String, out: String) = {
    val from = getClass().getResource("/" + folder +"/" + in)
    val dest = new File(out+in)
    println(folder + " " + out + " " + in + " " + dest)
    FileUtils.copyURLToFile(from, dest);
  }

  sealed trait CodegenDep {
    def folder: String
    def name: String
    def copy(out: String) = copyFile(folder, name, out)
  }

  case class AlwaysDep(folder: String, name: String = "") extends CodegenDep

  var dependencies: List[CodegenDep] = Nil
  var moveDependencies: List[CodegenDep] = Nil

  // FIXME: Should be OS-independent. Ideally want something that also supports wildcards, maybe recursive copy
  def copyDependencies(out: String): Unit = {
    // Files that need to cp
    dependencies.foreach{dep => 
      s"mkdir -p ${out}${java.io.File.separator}" !
    }
    dependencies.foreach{dep =>
      log("copy: " + dep )
      dep.copy(out)
    }
    // Files that need to mv
    moveDependencies.foreach{dep => 
      s"mkdir -p ${out}${java.io.File.separator}" !
    }
    moveDependencies.foreach{dep => 
      log(s"mv ${dep.folder} ${out}${java.io.File.separator}${dep.name}")
      s"mv ${dep.folder} ${out}${java.io.File.separator}${dep.name}" !
    }

  }
  override protected def postprocess[S:Type](b: Block[S]) = {
    copyDependencies(out)
    super.postprocess(b)
  }
}
