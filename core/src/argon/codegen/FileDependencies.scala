package argon.codegen

import sys.process._
import scala.language.postfixOps
import org.apache.commons.io._
import java.io.File
import java.util.zip.ZipInputStream

trait FileDependencies extends Codegen {
  import IR._

  sealed trait CodegenDep {
    def folder: String
    def name: String
    def copy(out: String): Unit
    def outputPath: Option[String]
  }

  case class FileDep(folder: String, name: String, outputPath: Option[String] = None) extends CodegenDep {
    def copy(out: String) = {
      val from = getClass.getResource("/" + folder +"/" + name)
      val outPath = outputPath.getOrElse(name)
      val dest = new File(out+outPath)
      new File(out).mkdirs()
      Console.println(folder + " " + out + " " + name + " " + dest)
      FileUtils.copyURLToFile(from, dest)
    }
  }

  case class DirDep(folder: String, name: String, outputPath: Option[String] = None) extends CodegenDep {
    override def copy(out: String) = {
      val dir = "/" + folder + "/" + name
      Console.println("Looking at " + dir)

      val src = getClass.getProtectionDomain.getCodeSource
      if (src != null) {
        val jar = src.getLocation
        val zip = new ZipInputStream(jar.openStream())

        Stream.continually(zip.getNextEntry)
          .takeWhile(_ != null)
          .map(_.getName)
          .filter(_.startsWith(folder + "/" + name))
          .filterNot(_.endsWith("/"))
          .map{e => FileDep(folder, e.split("/").drop(1).mkString("/"), outputPath) }
          .foreach(_.copy(out))
      }

    }
  }


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
