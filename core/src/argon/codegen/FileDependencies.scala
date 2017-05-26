package argon.codegen

import argon.core.compiler._
import sys.process._
import scala.language.postfixOps
import org.apache.commons.io._
import java.io.File

trait FileDependencies extends Codegen {

  sealed trait CodegenDep {
    def copy(out: String): Unit
  }

  case class FileDep(folder: String, name: String, relPath: String = "", outputPath:Option[String] = None) extends CodegenDep {
    def copy(out: String) = {
      val from = getClass.getResource("/" + folder +"/" + name)
      val outPathApp = outputPath.getOrElse(name)
      val relPathApp = relPath + outPathApp
      val dest = new File(out+relPathApp)
      // Console.println("source: /" + folder + "/" + name)
      // Console.println("from: " + from)
      // Console.println("dest: " + out + relPathApp)

      //Console.println(folder + " " + out + " " + name + " " + dest)
      //Console.println(from)
      try {
        val outPath = (out+relPathApp).split("/").dropRight(1).mkString("/")
        new File(outPath).mkdirs()
        FileUtils.copyURLToFile(from, dest)
      }
      catch {case e: NullPointerException =>
        error(s"Cannot copy file dependency $this: ")
        error("  src: " + folder + "/" + name)
        error("  dst: " + out + relPathApp)
        sys.exit(1)
      }
    }
  }

  case class DirDep(folder: String, name: String, relPath: String = "", outputPath:Option[String] = None) extends CodegenDep {
    override def copy(out: String) = {
      val dir = folder + "/" + name
      // Console.println("Looking at " + dir)

      def rename(e:String) = {
        val path = e.split("/").drop(2)
        if (outputPath.isDefined) {
          val sourceName = folder + "/" + path.dropRight(1).mkString("/")
          val outputName = outputPath.get + path.last
          FileDep(sourceName, path.last, relPath, Some(outputName))
        }
        else {
          val outputName = path.mkString("/")
          FileDep(folder, outputName, relPath)
        }
      }

      io.Source.fromURL(getClass.getResource("/files_list")).mkString("")
        .split("\n")
        .filter(_.startsWith("./"+dir))
        .map(rename)
        .foreach(_.copy(out))
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
