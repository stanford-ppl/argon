package argon.core

import argon.Config
import java.io.PrintStream
import org.virtualized.SourceContext
import java.nio.file.{Files, Paths}
import java.io.OutputStream

trait Reporting {
  class NullOutputStream() extends OutputStream {
    override def write(b: Int) { }
    override def write(b: Array[Byte]) { }
    override def write(b: Array[Byte], off: Int, len: Int) { }
  }
  val nullstream = new PrintStream(new NullOutputStream)

  private var logstream: PrintStream = nullstream
  private var _errors = 0
  private var _warns = 0
  def hadErrors = _errors > 0
  def nErrors = _errors
  def hadWarns = _warns > 0
  def nWarns = _warns

  def logError() { _errors += 1}

  def plural(x: Int, sing: String, plur: String): String = if (x == 1) sing else plur

  def createLog(dir: String, filename: String): PrintStream = {
    Files.createDirectories(Paths.get(dir))
    new PrintStream(dir + Config.sep + filename)
  }

  final def withLog[T](log: PrintStream)(blk: => T): T = {
    if (Config.verbosity >= 1) {
      val save = logstream
      logstream = log
      try {
        blk
      }
      finally {
        logstream.flush()
        logstream = save
      }
    }
    else blk
  }

  final def withLog[T](dir: String, filename: String)(blk: => T): T = {
    val log = createLog(dir, filename)
    try {
      withLog(log)(blk)
    }
    finally {
      log.close()
    }
  }
  final def withConsole[T](blk: => T): T = {
    val save = logstream
    logstream = System.out
    val result = blk
    logstream = save
    result
  }

  // TODO: Should these be macros?
  final def log(x: => Any): Unit = if (Config.verbosity >= 2) logstream.println(x)
  final def dbg(x: => Any): Unit = if (Config.verbosity >= 1) logstream.println(x)
  final def msg(x: => Any): Unit = {
    logstream.println(x)
    if (Config.verbosity >= 2) System.out.println(x)
  }

  final def report(x: => Any): Unit = if (Config.verbosity >= 0) System.out.println(x)
  final def warn(x: => Any): Unit = if (Config.showWarn) {
    System.err.println(s"[\u001B[33mwarn\u001B[0m] $x")
    log(s"[warn] $x")
  }
  final def error(x: => Any): Unit = {
    System.err.println(s"[\u001B[31merror\u001B[0m] $x")
    log(s"[error] $x")
  }

  final def warn(ctx: SourceContext, x: => Any, noWarn: Boolean = false): Unit = {
    warn(ctx.toString() + ": " + x)
    if (!noWarn) _warns += 1
  }
  final def error(ctx: SourceContext, x: => Any, noError: Boolean = false): Unit = {
    error(ctx.fileName + ":" + ctx.line + ": " + x)
    if (!noError) _errors += 1
  }

  final def warn(ctx: SourceContext, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    warn(ctx.lineContent.get)
    if (showCaret) warn(" "*(ctx.column-1) + "^") else warn("")
  }
  final def error(ctx: SourceContext, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    error(ctx.lineContent.get)
    if (showCaret) error(" "*(ctx.column-1) + "^") else error("")
  }

  final def warn(ctx: SourceContext): Unit = warn(ctx, showCaret = false)
  final def error(ctx: SourceContext): Unit = error(ctx, showCaret = false)

  def readable(x: Any): String = x match {
    case c:Class[_]    => c.getName.split('$').last.replace("class ", "")
    case p:Iterable[_] => if (p.isEmpty) "Nil" else p.map(readable).mkString("Seq(", ",", ")")
    case p:Product     => if (p.productIterator.isEmpty) c"${p.productPrefix}"
                          else c"""${p.productPrefix}(${p.productIterator.map(readable).mkString(", ")})"""
    case _ =>
      if (x == null) "null" else x.toString
  }

  def userReadable(x: Any): String = x match {
    case c:Class[_]    => c.getName.split('$').last.replace("class ", "")
    case _ => readable(x)
  }

  implicit class CompileReportHelper(sc: StringContext) {
    def c(args: Any*): String = sc.raw(args.map(readable): _*).stripMargin
  }
  implicit class FrontendReportHelper(sc: StringContext) {
    def u(args: Any*): String = sc.raw(args.map(userReadable): _*).stripMargin
  }

}
