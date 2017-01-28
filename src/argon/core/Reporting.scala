package argon.core

import argon.Config
import java.io.PrintStream
import org.virtualized.SourceContext
import java.nio.file.{Files, Paths}

trait Reporting {
  private var logstream: PrintStream = System.out
  private var _errors = 0
  def hadErrors = _errors > 0
  def nErrors = _errors

  def logError() { _errors += 1}

  def plural(x: Int, sing: String, plur: String): String = if (x == 1) sing else plur

  final def withLog[T](dir: String, filename: String)(blk: => T): T = {
    val save = logstream
    val logDir = dir + java.io.File.separator + Config.name
    Files.createDirectories(Paths.get(logDir))
    logstream = new PrintStream(s"$logDir" + java.io.File.separator + filename)
    try {
      blk
    }
    finally {
      logstream.flush()
      logstream.close()
      logstream = save
    }
  }
  final def withConsole[T](blk: => T): T = {
    val save = logstream
    logstream = System.out
    val result = blk
    logstream = save
    result
  }

  final def log(x: => Any): Unit = if (Config.verbosity >= 3) logstream.println(x)
  final def dbg(x: => Any): Unit = if (Config.verbosity >= 2) logstream.println(x)
  final def msg(x: => Any): Unit = if (Config.verbosity >= 1) logstream.println(x)

  final def report(x: => Any): Unit = if (Config.verbosity >= 0) System.out.println(x)
  final def warn(x: => Any): Unit = System.err.println(s"[\u001B[33mwarn\u001B[0m] $x")
  final def error(x: => Any): Unit = System.err.println(s"[\u001B[31merror\u001B[0m] $x")

  final def warn(ctx: SourceContext, x: => Any): Unit = warn(ctx.toString() + ": " + x)
  final def error(ctx: SourceContext, x: => Any): Unit = {
    error(ctx.fileName + ":" + ctx.line + ": " + x)
    _errors += 1
  }

  final def warn(ctx: SourceContext): Unit = if (ctx.lineContent.isDefined) {
    warn(ctx.lineContent.get)
    warn(" "*(ctx.column-1) + "^")
  }
  final def error(ctx: SourceContext): Unit = if (ctx.lineContent.isDefined) {
    error(ctx.lineContent.get)
    error(" "*(ctx.column-1) + "^")
  }

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
    case _ =>
      if (x == null) "null" else x.toString
  }

  implicit class CompileReportHelper(sc: StringContext) {
    def c(args: Any*): String = sc.raw(args.map(readable): _*).stripMargin
  }
  implicit class FrontendReportHelper(sc: StringContext) {
    def u(args: Any*): String = sc.raw(args.map(userReadable): _*).stripMargin
  }

}
