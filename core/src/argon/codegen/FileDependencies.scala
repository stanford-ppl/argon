package argon.codegen

import sys.process._
import scala.language.postfixOps
import org.apache.commons.io._
import java.io.File
import java.util.zip.ZipInputStream

trait FileDependencies extends Codegen {
  import IR._

  sealed trait CodegenDep {
    def copy(out: String): Unit
  }

  case class FileDep(folder: String, name: String, relPath: String = "", outputPath:Option[String] = None) extends CodegenDep {
    def copy(out: String) = {
      val from = getClass.getResource("/" + folder +"/" + name)
      val outPathApp = outputPath.getOrElse(name)
      val relPathApp = relPath + outPathApp
      val dest = new File(out+relPathApp)
      new File(out).mkdirs()
      Console.println(folder + " " + out + " " + name + " " + dest)
      Console.println(from)
      FileUtils.copyURLToFile(from, dest)
    }
  }

  case class DirDep(folder: String, name: String, relPath: String = "", outputPath:Option[String] = None) extends CodegenDep {
    override def copy(out: String) = {
      val dir = "/" + folder + "/" + name
      // Console.println("Looking at " + dir)

      val src = getClass.getProtectionDomain.getCodeSource
      if (src != null) {
        val jar = src.getLocation
        val zip = new ZipInputStream(jar.openStream())

        def rename(e:String) = {
          val path = e.split("/").drop(1)
          outputPath.map(_+path.last).getOrElse(path.mkString("/"))
        }

        Stream.continually(zip.getNextEntry)
          .takeWhile(_ != null)
          .map(_.getName)
          .filter(_.startsWith(folder + "/" + name))
          .filterNot(_.endsWith("/"))
          .map(rename)
          .map(e => FileDep(folder, e, relPath) )
          .foreach(_.copy(out))
      }

    }
  }


  var dependencies: List[CodegenDep] = Nil

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


  }
  override protected def postprocess[S:Type](b: Block[S]) = {
    copyDependencies(out)
    super.postprocess(b)
  }
}
