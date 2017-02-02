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

  protected var stream: PrintWriter = _
  protected var streamName = ""
  protected var streamTab = collection.mutable.Map[String, Int]() // Map from filename to its tab level
  protected var streamMap = collection.mutable.Map[PrintWriter, String]() // Map from PrintWriter to its string name
  protected var streamMapReverse = collection.mutable.Map[String, PrintWriter]() // Map from PrintWriter to its string name
  protected val tabWidth: Int = 2

  private def tabbed: String = " "*(tabWidth*(streamTab getOrElse (streamName, 0)))

  final protected def emit(x: String): Unit = { stream.println(tabbed + x) }
  final protected def open(x: String): Unit = { stream.println(tabbed + x); if (streamTab contains streamName) streamTab(streamName) += 1 }
  final protected def close(x: String): Unit = { if (streamTab contains streamName) streamTab(streamName) -= 1; stream.println(tabbed + x)  }

  final protected def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    val saveName = streamMap getOrElse (stream,"")
    stream = out
    streamName = streamMap(out)
    try { body } finally { stream.flush(); stream = save; streamName = saveName }
  }

  final protected def newStream(name: String): PrintWriter = {
    // TODO: Assert streamMap does not contain this guy already
    streamTab += (name -> 0)
    Files.createDirectories(Paths.get(out))
    val file = new PrintWriter(s"${out}${name}.$ext")
    streamMap += (file -> name)
    streamMapReverse += (name -> file)
    file      
  }

  final protected def getStream(name: String): PrintWriter = { // Use stream if it exists, otherwise maek it exist
    // TODO: Assert streamMap does not contain this guy already
    if (streamMapReverse.contains(name)) {
      streamMapReverse(name)
    } else {
      newStream(name)
    }
  }

  protected def remap(tp: Staged[_]): String = tp.toString
  protected def quoteConst(c: Const[_]): String = throw new ConstantGenFailedException(c)
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
  protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = throw new GenerationFailedException(rhs)
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
