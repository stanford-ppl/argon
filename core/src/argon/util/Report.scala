package argon.util

import argon.core.{Config, _}
import forge._
import java.io.PrintStream
import java.nio.file.{Files, Paths}

import org.virtualized.{SourceContext => SrcCtx}

object Report {
  def plural(x: Int, sing: String, plur: String): String = if (x == 1) sing else plur

  def createLog(dir: String, filename: String): PrintStream = {
    Files.createDirectories(Paths.get(dir))
    new PrintStream(dir + Config.sep + filename)
  }

  @stateful def withLog[T](log: PrintStream)(blk: => T)(implicit state: State): T = {
    if (Config.verbosity >= 1) {
      val save = state.logstream
      state.logstream = log
      try {
        blk
      }
      finally {
        state.logstream.flush()
        state.logstream = save
      }
    }
    else blk
  }

  @stateful def withLog[T](dir: String, filename: String)(blk: => T)(implicit state: State): T = {
    val log = createLog(dir, filename)
    try {
      withLog(log)(blk)
    }
    finally {
      log.close()
    }
  }

  // TODO: Should these be macros?
  @stateful def log(x: => Any)(implicit state: State): Unit = if (Config.verbosity >= 2) state.logstream.println(x)
  @stateful def dbg(x: => Any)(implicit state: State): Unit = if (Config.verbosity >= 1) state.logstream.println(x)
  @stateful def msg(x: => Any, level: Int = 2)(implicit state: State): Unit = {
    state.logstream.println(x)
    if (Config.verbosity >= level) System.out.println(x)
  }

  def report(x: => Any): Unit = if (Config.verbosity >= 0) System.out.println(x)
  def warn(x: => Any): Unit = if (Config.showWarn) {
    System.err.println(s"[\u001B[33mwarn\u001B[0m] $x")
  }
  def error(x: => Any): Unit = if (Config.verbosity >= -1) {
    System.err.println(s"[\u001B[31merror\u001B[0m] $x")
  }
  def bug(x: => Any): Unit = {
    System.err.println(s"[\u001B[35mbug\u001B[0m] $x")
  }

  @stateful def warn(ctx: SrcCtx, x: => Any, noWarn: Boolean = false)(implicit state: State): Unit = {
    warn(ctx.toString + ": " + x)
    if (!noWarn) state.logWarning()
  }
  @stateful def error(ctx: SrcCtx, x: => Any, noError: Boolean = false)(implicit state: State): Unit = {
    error(ctx.fileName + ":" + ctx.line + ": " + x)
    if (!noError) state.logError()
  }
  @stateful def bug(ctx: SrcCtx, x: => Any, noError: Boolean = false)(implicit state: State): Unit = {
    bug(ctx.fileName + ":" + ctx.line + ": " + x)
    if (!noError) state.logError()
  }

  def warn(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    warn(ctx.lineContent.get)
    if (showCaret) warn(" "*(ctx.column-1) + "^") else warn("")
  }
  def error(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    error(ctx.lineContent.get)
    if (showCaret) error(" "*(ctx.column-1) + "^") else error("")
  }
  def bug(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    error(ctx.lineContent.get)
    if (showCaret) bug(" "*(ctx.column-1) + "^") else error("")
  }

  def warn(ctx: SrcCtx): Unit = warn(ctx, showCaret = false)
  def error(ctx: SrcCtx): Unit = error(ctx, showCaret = false)
  def bug(ctx: SrcCtx): Unit = bug(ctx, showCaret = false)

  @stateful def str(lhs: Exp[_]): String = lhs match {
    case Def(rhs) => readable(lhs) + " = " + readable(rhs)
    case Const(c) => readable(lhs) + " = " + readable(c)
    case _: Bound[_] => readable(lhs) + " [bound]"
  }
  @stateful def str(lhs: Seq[Exp[_]]): String = lhs.head match {
    case Def(rhs) => readable(lhs) + " = " + readable(rhs)
    case syms => readable(syms)
  }

  def readable(x: Any): String = x match {
    case c: CompilerFacing => c.toStringCompiler
    case x: Tuple3[_,_,_]  => s"${readable(x._1)} = ${readable(x._2)} [inputs = ${readable(x._3)}]"
    case c: Class[_]       => c.getName.split('$').last.replace("class ", "").split('.').last
    case p: Iterable[_]    => if (p.isEmpty) "Nil" else p.map(readable).mkString("Seq(", ",", ")")
    case _ =>
      if (x == null) "null" else x.toString
  }

  def userReadable(x: Any): String = x match {
    case c: FrontendFacing => c.toStringFrontend
    case c: Class[_]  => c.getName.split('$').last.replace("class ", "").split('.').last
    case _ => readable(x)
  }

  implicit class CompilerReportHelper(sc: StringContext) {
    def c(args: Any*): String = sc.raw(args.map(Report.readable): _*).stripMargin
  }
  implicit class FrontendReportHelper(sc: StringContext) {
    def u(args: Any*): String = sc.raw(args.map(Report.userReadable): _*).stripMargin
  }
}