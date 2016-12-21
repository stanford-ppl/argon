package argon.codegen

import argon.traversal.Traversal
import argon.core.Statements

import java.io.PrintWriter

trait Codegen extends Traversal {
  val IR: Statements
  import IR._

  override val recurse: RecurseOpt = Never

  val lang: String = "generic"
  val ext: String = "generic"

  var stream: PrintWriter = _
  protected var streamTab: Int = 0
  protected val tabWidth: Int = 2

  private def tabbed: String = " "*(tabWidth*streamTab)

  final def emit(x: String): Unit = { stream.println(tabbed + x) }
  final def open(x: String): Unit = { stream.println(tabbed + x); streamTab += 1 }
  final def close(x: String): Unit = { streamTab -= 1; stream.println(tabbed + x)  }

  final def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush(); stream = save }
  }

  def remap(tp: Staged[_]): String = tp.toString
  def quoteConst(c: Const[_]): String = throw new ConstantGenFailedException(c)
  def quote(s: Sym[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case s: Sym[_] => s"x${s.id}"
  }

  def quoteOrRemap(arg: Any): String = arg match {
    case e: Sym[_] => quote(e)
    case m: Staged[_] => remap(m)
    case s: String => s
    case _ => throw new RuntimeException(s"Could not quote or remap $arg")
  }

  def emitBlock(b: Block[_]): Unit = traverseBlock(b)
  def emitNode(lhs: Sym[_], rhs: Op[_]) = throw new GenerationFailedException(rhs)
  def emitFat(lhs: List[Sym[_]], rhs: Def) = throw new GenerationFailedException(rhs)

  override def visit(lhs: Sym[_], rhs: Op[_]) = emitNode(lhs, rhs)
  override def visitFat(lhs: List[Sym[_]], rhs: Def) = emitFat(lhs, rhs)

  def emitFileHeader(): Unit = { }
  def emitFileFooter(): Unit = { }

  // Provides automatic quoting and remapping in the src string interpolator
  // emit(src"val $x = $rhs")
  implicit class CodegenHelper(sc: StringContext) {
    def src(args: Any*): String = sc.raw(args.map(quoteOrRemap): _*).stripMargin
  }
}
