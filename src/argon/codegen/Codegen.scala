package argon.codegen

import argon.traversal.Traversal
import argon.core.Statements
import argon.Config

import java.io.PrintWriter

trait Codegen extends Traversal {
  val IR: Statements
  import IR._

  override val recurse: RecurseOpt = Never

  val lang: String
  val ext: String
  def out: String = s"${Config.genDir}${java.io.File.separator}${Config.name}${java.io.File.separator}$lang${java.io.File.separator}"

  protected var stream: PrintWriter = _
  protected var streamTab: Int = 0
  protected val tabWidth: Int = 2

  private def tabbed: String = " "*(tabWidth*streamTab)

  final protected def emit(x: String): Unit = { stream.println(tabbed + x) }
  final protected def open(x: String): Unit = { stream.println(tabbed + x); streamTab += 1 }
  final protected def close(x: String): Unit = { streamTab -= 1; stream.println(tabbed + x)  }

  final protected def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush(); stream = save }
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

  protected def emitBlock(b: Block[_]): Unit = traverseBlock(b)
  protected def emitLambda(b: Lambda[_]): Unit = traverseLambda(b)
  protected def emitScope(b: Scope[_]): Unit = traverseScope(b)
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
