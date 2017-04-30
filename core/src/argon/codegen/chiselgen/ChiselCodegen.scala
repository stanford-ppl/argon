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

  var gw_lines = 0
  final protected def emitGlobalWire(x: String, forceful: Boolean = false): Unit = { 
    if (x.indexOf("val") == 0) gw_lines = gw_lines + 1
    val file_num = (numGlobalFiles-1) min {gw_lines / maxLinesPerFile}
    withStream(getStream("GlobalWires" + file_num)) {
      emit(x, forceful) 
    }
  }

  var gm_lines = 0
  final protected def emitGlobalModule(x: String, forceful: Boolean = false): Unit = { 
    if (x.indexOf("val") == 0) gm_lines = gm_lines + 1
    val file_num = (numGlobalFiles-1) min {gm_lines / maxLinesPerFile}
    withStream(getStream("GlobalModules" + file_num)) {
      emit(x, forceful) 
    }
  }

  final protected def emitGlobalRetiming(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalRetiming")) {
      emit(x, forceful) 
    }
  }

  final protected def openGlobalWire(x: String, forceful: Boolean = false): Unit = { 
    if (x.indexOf("val") == 0) gw_lines = gw_lines + 1
    val file_num = (numGlobalFiles-1) min {gw_lines / maxLinesPerFile}
    withStream(getStream("GlobalWires"+file_num)) {
      open(x, forceful) 
    }
  }

  final protected def openGlobalModule(x: String, forceful: Boolean = false): Unit = { 
    if (x.indexOf("val") == 0) gm_lines = gm_lines + 1
    val file_num = (numGlobalFiles-1) min {gm_lines / maxLinesPerFile}
    withStream(getStream("GlobalModules"+file_num)) {
      open(x, forceful) 
    }
  }

  final protected def openGlobalRetiming(x: String, forceful: Boolean = false): Unit = { 
    withStream(getStream("GlobalRetiming")) {
      open(x, forceful) 
    }
  }

  final protected def closeGlobalWire(x: String, forceful: Boolean = false): Unit = { 
    if (x.indexOf("val") == 0) gw_lines = gw_lines + 1
    val file_num = (numGlobalFiles-1) min {gw_lines / maxLinesPerFile}
    withStream(getStream("GlobalWires"+file_num)) {
      close(x, forceful) 
    }
  }

  final protected def closeGlobalModule(x: String, forceful: Boolean = false): Unit = { 
    if (x.indexOf("val") == 0) gm_lines = gm_lines + 1
    val file_num = (numGlobalFiles-1) min {gm_lines / maxLinesPerFile}
    withStream(getStream("GlobalModules"+file_num)) {
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


  def tabbing(stream: String): String = " "*(tabWidth*(streamTab getOrElse (stream, 0)))

  protected def strip_ext(name: String): String = {"\\..*".r.replaceAllIn(name,"")}
  protected def get_ext(name: String): String = {".*\\.".r.replaceAllIn(name,"")}
  protected def get_real_stream(curStream: String, x: String): String = {
    if ((curStream contains "Global") | (curStream contains "BufferControl") | (curStream contains "IOModule") | (curStream contains "AccelTop")) {
      strip_ext(curStream)
    } else {
      val current_ext = streamExtensions(strip_ext(curStream)).last
      val cur_stream_ext = if (current_ext == 0) {strip_ext(curStream)} else {strip_ext(curStream) + "_" + current_ext}
      val cur_tabbing = streamTab(cur_stream_ext + "." + get_ext(curStream))
      if (/*(x.indexOf("val") == 0) & */(cur_tabbing == 1)) streamLines(cur_stream_ext) += 1
      val file_num = streamLines(cur_stream_ext) / maxLinesPerFile
      if (streamLines(cur_stream_ext) % maxLinesPerFile == 0 & (!streamExtensions(strip_ext(curStream)).contains(file_num))) { // How the fuck is it entering this loop if the condition is false
        val next = newStream(strip_ext(curStream) + "_" + file_num)
        val curlist = streamExtensions(strip_ext(curStream))
        streamExtensions += (strip_ext(curStream) -> {curlist :+ file_num})
        withStream(next) {
          stream.println("""package accel
import templates._
import templates.ops._
import types._
import chisel3._
import chisel3.util._""")
          val prnt = if (file_num == 1) src"${strip_ext(curStream)}" else src"${strip_ext(curStream)}_${file_num-1}"
          open(src"""trait ${strip_ext(curStream)}_${file_num} extends ${prnt} {""")
        }
      }
      cur_stream_ext
    }

  }



  override protected def emit(x: String, forceful: Boolean = false): Unit = { 
    if (emitEn | forceful) {
      val realstream = get_real_stream(streamName,x)
      withStream(getStream(realstream)) {stream.println(tabbing(realstream + "." + get_ext(streamName)) + x)}
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of ${x} does not belong in this backend")}
    }
  } 
  override protected def open(x: String, forceful: Boolean = false): Unit = {
    if (emitEn | forceful) {
      val realstream = get_real_stream(streamName,x)
      withStream(getStream(realstream)) {stream.println(tabbing(realstream + "." + get_ext(streamName)) + x)}; 
      if (streamTab contains {realstream + "." + get_ext(streamName)}) streamTab(realstream + "." + get_ext(streamName)) += 1 
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of ${x} does not belong in this backend")}
    }
  }
  override protected def close(x: String, forceful: Boolean = false): Unit = { 
    if (emitEn | forceful) {
      val realstream = get_real_stream(streamName,x)
      if (streamTab contains {realstream + "." + get_ext(streamName)}) {
        streamTab(realstream + "." + get_ext(streamName)) -= 1; 
        withStream(getStream(realstream)) {stream.println(tabbing(realstream + "." + get_ext(streamName)) + x)}
      }
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of ${x} does not belong in this backend")}
    }
  } 
  override protected def closeopen(x: String, forceful: Boolean = false): Unit = { // Good for "} else {" lines
    if (emitEn | forceful) {
      val realstream = get_real_stream(streamName,x)
      if (streamTab contains {realstream + "." + get_ext(streamName)}) {
        streamTab(realstream + "." + get_ext(streamName)) -= 1; 
        withStream(getStream(realstream)) {stream.println(x);}
        streamTab(realstream + "." + get_ext(streamName)) += 1
      }
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen-NOTE ] Emission of ${x} does not belong in this backend")}
    }
  } 


  final protected def withSubStream[A](name: String, parent: String, inner: Boolean = false)(body: => A): A = { // Places body inside its own trait file and includes it at the end
    if (Config.multifile == 4) {
      val prnts = if (!(streamExtensions contains parent)) src"$parent" else streamExtensions(parent).map{i => if (i == 0) src"$parent" else src"${parent}_${i}"}.mkString(" with ")
      emit(src"// Creating sub kernel ${name}")
      withStream(newStream(name)) {
          emit("""package accel
import templates._
import templates.ops._
import types._
import chisel3._
import chisel3.util._""")
          open(src"""trait ${name} extends ${prnts} {""")
          try { body } 
          finally { 
            streamExtensions(name).foreach{i => 
              val fname = if (i == 0) src"$name" else src"${name}_${i}"
              withStream(getStream(fname)) { stream.println("}")}
            }
          }
      }
    } else if (Config.multifile == 3 & inner) {
        withStream(newStream(name)) {
            emit("""package accel
  import templates._
  import templates.ops._
  import types._
  import chisel3._
  import chisel3.util._""")
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
