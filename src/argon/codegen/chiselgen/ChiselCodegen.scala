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

  final protected def emitGlobal(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalWires")) {
      emit(x, forceful) 
    }
  }

  protected def bitWidth(tp: Staged[_]): Int = {
    c"$tp" match {
      case "Avalon" => 32
      case _ => throw new NoBitWidthException(tp)
    }
  }


  final protected def emitModule(lhs: String, x: String, args: String*): Unit = {
    // dependencies ::= AlwaysDep(s"""${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/template-level/templates/$x.scala""")

    emit(src"""val $lhs = Module(new ${x}(${args.mkString}))""")
  } 

  override def copyDependencies(out: String): Unit = {
    // s"mkdir ${out}${java.io.File.separator}templates" !
    // s"mkdir ${out}${java.io.File.separator}templates".!
    // dependencies.foreach{dep => if (dep.needsCopy) {
    //   log(s"Copying ${dep.input} to $out")
    //   s"cp ${dep.input} ${out}${java.io.File.separator}templates${java.io.File.separator}${dep.outputPath}" !
    // }}
    val resourcesPath = s"${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources"
    dependencies ::= AlwaysDep(s"""${resourcesPath}/template-level/templates""")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/template-level/fringeHW""")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/template-level/fringeZynq""")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/template-level/fringeDE1SoC""")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/template-level/fringeVCS""")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/Makefile""", "..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/verilator.mk""", "..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/zynq.mk""", "..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/vcs.mk""", "..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/in.txt""", "..")
    // dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/direct-test.sh""", "..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/build.sbt""", "..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/run.sh""","..")
    dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/Top.scala""")
    // dependencies ::= AlwaysDep(s"""${resourcesPath}/app-level/app-test""")
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
          open(src"""trait ${name} extends ${parent.replace("AccelController","RootController")} {""")
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
