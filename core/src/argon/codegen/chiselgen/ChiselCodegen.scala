package argon.codegen.chiselgen

import sys.process._
import scala.language.postfixOps
import argon.codegen.Codegen
import argon.Config
import scala.collection.mutable.HashMap
import argon.codegen.FileDependencies

trait ChiselCodegen extends Codegen with FileDependencies { // FileDependencies extends Codegen already
  import IR._
  override val name = "Chisel Codegen"
  override val lang: String = "chisel"
  override val ext: String = "scala"
  var controllerStack = scala.collection.mutable.Stack[Exp[_]]()

  var alphaconv = HashMap[String, String]() // Map for tracking defs of nodes and if they get redeffed anywhere, we map it to a suffix

  final def alphaconv_register(xx: String): Unit = {
    val x = "_reuse.*".r.replaceAllIn(xx, "")
    if (alphaconv.contains(x)) {
      val suf = alphaconv(x).replace("_reuse","")
      if (suf == "") {
        alphaconv += (x -> "_reuse1") // If already used, increment suffix  
      } else {
        val newsuf = suf.toInt + 1
        alphaconv += (x -> s"_reuse$newsuf")
      }
    } else {
      alphaconv += (x -> "") // Otherwise don't suffix it
    }
  }

  override protected def quoteOrRemap(arg: Any): String = arg match {
    case e: Exp[_] => quote(e) + alphaconv.getOrElse(quote(e), "")
    case _ => super.quoteOrRemap(arg)
  }

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"// results in ${b.result}")
  }

  final protected def emitController(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"// results in ${b.result}")
  }

  override def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case lhs: Sym[_] => s"x${lhs.id}"
  }

  final protected def emitGlobalWire(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalWires")) {
      emit(x, forceful) 
    }
  }

  final protected def emitGlobalModule(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalModules")) {
      emit(x, forceful) 
    }
  }

  final protected def emitGlobalRetiming(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalRetiming")) {
      emit(x, forceful) 
    }
  }

  final protected def openGlobalWire(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalWires")) {
      open(x, forceful) 
    }
  }

  final protected def openGlobalModule(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalModules")) {
      open(x, forceful) 
    }
  }

  final protected def openGlobalRetiming(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalRetiming")) {
      open(x, forceful) 
    }
  }

  final protected def closeGlobalWire(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalWires")) {
      close(x, forceful) 
    }
  }

  final protected def closeGlobalModule(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalModules")) {
      close(x, forceful) 
    }
  }

  final protected def closeGlobalRetiming(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalRetiming")) {
      close(x, forceful) 
    }
  }

  protected def bitWidth(tp: Type[_]): Int = {
    c"$tp" match {
      case "Avalon" => 32
      case _ => throw new NoBitWidthException(tp)
    }
  }

  final protected def emitModule(lhs: String, x: String, args: String*): Unit = {
    // dependencies ::= AlwaysDep("chiselgen", "template-level/templates/$x.scala")

    emit(src"""val $lhs = Module(new ${x}(${args.mkString}))""")
  } 

  override def copyDependencies(out: String): Unit = {
    // s"mkdir ${out}${java.io.File.separator}templates" !
    // s"mkdir ${out}${java.io.File.separator}templates".!
    // dependencies.foreach{dep => if (dep.needsCopy) {
    //   log(s"Copying ${dep.input} to $out")
    //   s"cp ${dep.input} ${out}${java.io.File.separator}templates${java.io.File.separator}${dep.outputPath}" !
    // }}
    val resourcesPath = s"chiselgen"

    dependencies ::= DirDep(resourcesPath, "template-level/templates")
    dependencies ::= DirDep(resourcesPath, "template-level/fringeHW") 
    dependencies ::= DirDep(resourcesPath, "template-level/fringeZynq")
    dependencies ::= DirDep(resourcesPath, "template-level/fringeDE1SoC")
    dependencies ::= DirDep(resourcesPath, "template-level/fringeVCS")
    
    dependencies ::= FileDep(resourcesPath, "app-level/Makefile", "../", Some("Makefile")) 
    dependencies ::= FileDep(resourcesPath, "app-level/verilator.mk", "../", Some("verilator.mk"))
    dependencies ::= FileDep(resourcesPath, "app-level/zynq.mk", "../", Some("zynq.mk"))
    dependencies ::= FileDep(resourcesPath, "app-level/de1soc.mk", "../", Some("de1soc.mk"))
    dependencies ::= FileDep(resourcesPath, "app-level/vcs.mk", "../", Some("vcs.mk"))
    dependencies ::= FileDep(resourcesPath, "app-level/build.sbt", "../", Some("build.sbt"))
    dependencies ::= FileDep(resourcesPath, "app-level/run.sh", "../", Some("run.sh"))
    dependencies ::= FileDep(resourcesPath, "app-level/Top.scala", outputPath = Some("Top.scala")) 
    super.copyDependencies(out)
  }


  final protected def withSubStream[A](name: String, parent: String, inner: Boolean = false)(body: => A): A = { // Places body inside its own trait file and includes it at the end
    if (Config.multifile == 4) {
      emit(src"// Creating sub kernel ${name}")
      withStream(newStream(name)) {
          emit("""package accel
import templates._
import templates.ops._
import types._
import chisel3._""")
          open(src"""trait ${name} extends ${parent} {""")
          try { body } 
          finally { 
            close("}")
          }
      }
    } else if (Config.multifile == 3 & inner) {
        withStream(newStream(name)) {
            emit("""package accel
  import templates._
  import templates.ops._
  import types._
  import chisel3._""")
            open(src"""trait ${name} extends RootController {""")
            try { body } 
            finally { 
              close("}")
            }
        }
      
    } else if (Config.multifile == 2) {
      open(src";{ // Multifile disabled, emitting $name kernel here")
      try { body } 
      finally { close("}") }
    } else if (Config.multifile == 1 & inner) {
      open(src";{ // Multifile disabled, emitting $name kernel here")
      try { body } 
      finally { close("}") }
    } else {
      open(src"// Multifile disabled, emitting $name kernel here without scoping")
      try { body } 
      finally { close("") }      
    }
  }





}
