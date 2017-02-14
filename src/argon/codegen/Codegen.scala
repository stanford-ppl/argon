package argon.codegen

import argon.traversal.Traversal
import argon.core.Staging
import argon.Config

import java.nio.file.{Files, Paths}
import java.io.PrintWriter

trait Codegen extends Traversal {
  val IR: Staging
  import IR._

  override val recurse: RecurseOpt = Never

  val lang: String
  val ext: String
  def out: String = s"${Config.genDir}${Config.sep}$lang${Config.sep}"
  var emitEn: Boolean = true // Hack for masking Cpp from FPGA gen, usually always true except for chisel and cpp gen

  var stream: PrintWriter = _
  var streamName = ""
  var streamTab = collection.mutable.Map[String, Int]() // Map from filename to its tab level
  var streamMap = collection.mutable.Map[PrintWriter, String]() // Map from PrintWriter to its string name
  var streamMapReverse = collection.mutable.Map[String, PrintWriter]() // Map from PrintWriter to its string name
  var topLayerTraits = List[String]() // List of top layer nodes, used in multifile==4 generation
  val tabWidth: Int = 2

  def tabbed: String = " "*(tabWidth*(streamTab getOrElse (streamName, 0)))

  protected def emit(x: String, forceful: Boolean = false): Unit = { 
    if (emitEn | forceful) {
      stream.println(tabbed + x)
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen ] Emission of ${x} does not belong in this backend")}
    }
  } 
  protected def open(x: String): Unit = {
    if (emitEn) {
      stream.println(tabbed + x); if (streamTab contains streamName) streamTab(streamName) += 1 
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen ] Emission of ${x} does not belong in this backend")}
    }
  }
  protected def close(x: String): Unit = { 
    if (emitEn) {
      if (streamTab contains streamName) streamTab(streamName) -= 1; stream.println(tabbed + x)
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen ] Emission of ${x} does not belong in this backend")}
    }
  } 
  protected def closeopen(x: String): Unit = { // Good for "} else {" lines
    if (emitEn) {
      if (streamTab contains streamName) streamTab(streamName) -= 1; stream.println(tabbed + x); streamTab(streamName) += 1
    } else { 
      if (Config.emitDevel == 2) {Console.println(s"[ ${lang}gen ] Emission of ${x} does not belong in this backend")}
    }
  } 

  final protected def toggleEn(): Unit = {
    if (emitEn) {
      if (Config.emitDevel == 2) {
        Console.println(s"[ ${lang}gen ] Disabling emits")
      }
      emitEn = false
    } else {
      if (Config.emitDevel == 2) {
        Console.println(s"[ ${lang}gen ] Enabling emits")
      }
      emitEn = true      
    }
  }

  final protected def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    val saveName = streamMap getOrElse (stream,"")
    stream = out
    streamName = streamMap(out)
    try { body } finally { stream.flush(); stream = save; streamName = saveName }
  }

  final protected def newStream(name: String, exten: String = ext): PrintWriter = {
    // TODO: Assert streamMap does not contain this guy already
    val fullname = name + "." + exten
    streamTab += (fullname -> 0)
    Files.createDirectories(Paths.get(out))
    val file = new PrintWriter(s"${out}${name}.$exten")
    streamMap += (file -> fullname)
    streamMapReverse += (fullname -> file)
    file      
  }

  final protected def getStream(name: String, exten: String = ext): PrintWriter = { // Use stream if it exists, otherwise maek it exist
    // TODO: Assert streamMap does not contain this guy already
    val fullname = name + "." + exten
    if (streamMapReverse.contains(fullname)) {
      streamMapReverse(fullname)
    } else {
      newStream(name, exten)
    }
  }

  protected def remap(tp: Staged[_]): String = tp.toString
  protected def quoteConst(c: Const[_]): String = {
    if (Config.emitDevel > 0) {
      if (emitEn) { // Want to emit but can't
        Console.println(s"[ ${lang}gen ] No quote for $c")  
      } else { // No need to emit
        Console.println(s"[ ${lang}gen ] Quoting of $c does not belong in this backend")
      }
      
      ""
    } else {
      throw new ConstantGenFailedException(c)
    }
  }
  protected def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case s: Sym[_] => s"x${s.id}"
  }

  protected def quoteOrRemap(arg: Any): String = arg match {
    case e: Exp[_] => quote(e)
    case m: Staged[_] => remap(m)
    case s: String => s
    case c: Int => c.toString
    case _ => throw new RuntimeException(s"Could not quote or remap $arg")
  }

  protected def emitBlock(b: Block[_]): Unit = visitBlock(b)
  protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (emitEn) {
      if (Config.emitDevel == 0) {
        throw new GenerationFailedException(rhs)
      } else {
        Console.println(s"[ WARN ] no backend for $lhs = $rhs in $lang")  
      } 
    } else {
      if (Config.emitDevel == 2) Console.println(s"[ ${lang}gen ] Emission of ${lhs} = $rhs does not belong in this backend")
    }
  }

  protected def emitFat(lhs: List[Sym[_]], rhs: Def): Unit = throw new GenerationFailedException(rhs)

  protected def emitFileHeader(): Unit = { }
  protected def emitFileFooter(): Unit = { }

  // Provides automatic quoting and remapping in the src string interpolator
  // emit(src"val $x = $rhs")
  implicit class CodegenHelper(sc: StringContext) {
    def src(args: Any*): String = sc.raw(args.map(quoteOrRemap): _*).stripMargin
  }

  final override protected def visit(lhs: Sym[_], rhs: Op[_]) = emitNode(lhs, rhs)
  final override protected def visitFat(lhs: List[Sym[_]], rhs: Def) = emitFat(lhs, rhs)
}
