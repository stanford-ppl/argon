package argon.codegen.chiselgen

import sys.process._
import scala.language.postfixOps
import argon.codegen.Codegen
import argon.Config
import argon.codegen.FileDependencies


trait ChiselCodegen extends Codegen with FileDependencies { // FileDependencies extends Codegen already
  import IR._
  override val name = "Chisel Codegen"
  override val lang: String = "chisel"
  override val ext: String = "scala"

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

  final protected def emitGlobal(x: String): Unit = { 
    withStream(getStream("GlobalWires")) {
      emit(x) 
    }
  }

  protected def bitWidth(tp: Staged[_]): Int = {
    throw new NoBitWidthException(tp)
  }


  final protected def emitModule(lhs: String, x: String, args: String*): Unit = {
    // dependencies ::= AlwaysDep(s"""${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/template-level/templates/$x.scala""")

    emit(src"""val $lhs = Module(new ${x}(${args.mkString}))""")
  } 

  protected def hasFracBits(tp: Staged[_]): Boolean = false

  override def copyDependencies(out: String): Unit = {
    // FIXME: Should be OS-independent. Ideally want something that also supports wildcards, maybe recursive copy
    // s"mkdir ${out}${java.io.File.separator}templates" !
    // s"mkdir ${out}${java.io.File.separator}templates".!
    // dependencies.foreach{dep => if (dep.needsCopy) {
    //   log(s"Copying ${dep.input} to $out")
    //   s"cp ${dep.input} ${out}${java.io.File.separator}templates${java.io.File.separator}${dep.outputPath}" !
    // }}
    s"""cp -r ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/template-level/templates ${out}""".!
    s"""cp ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/app-level/Makefile ${out}/..""".!
    s"""cp ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/app-level/direct-test.sh ${out}/..""".!
    s"""cp ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/app-level/build.sbt ${out}/..""".!
    s"""cp ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/app-level/run.sh ${out}/..""".!
    s"""cp -r ${sys.env("SPATIAL_HOME")}/src/spatial/codegen/chiselgen/resources/app-level/app-test ${out}""".!
    super.copyDependencies(out)
  }


  final protected def withSubStream[A](name: String, parent: String, inner: Boolean = false)(body: => A): A = { // Places body inside its own trait file and includes it at the end
    if (Config.multifile == 4) {
      emit("// Creating sub kernel")
      emit(src"""// create_${name}()""")
      withStream(newStream(name)) {
          emit("""package app
import templates._
import chisel3._""")
          open(s"""trait ${name} extends TopTrait /*${parent.replace("AccelController","TopTrait")}*/ {""")
          open(s"""def create_${name}() {""")
          try { body } 
          finally { 
            close("}")
            close("}")
          }
      }
    } else if (Config.multifile == 3 & inner) {
        withStream(newStream(name)) {
            emit("""package app
  import templates._
  import chisel3._""")
            open(s"""trait ${name} extends GlobalWires with TopTrait {""")
            open(s"""def create_${name}() {""")
            try { body } 
            finally { 
              close("}")
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
