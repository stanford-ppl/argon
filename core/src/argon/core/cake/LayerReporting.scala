package argon.core.cake

import argon.util._
import forge._

import java.io.PrintStream
import java.nio.file.{Files, Paths}

trait LayerReporting { self: ArgonCake =>
  type Log = java.io.PrintStream

  def plural(x: Int, singular: String, plur: String): String = Report.plural(x, singular, plur)

  def createLog(dir: String, filename: String): PrintStream = {
    Files.createDirectories(Paths.get(dir))
    new PrintStream(dir + "/" + filename)
  }

  @stateful def withConsole[T](blk: => T): T = withLog(System.out)(blk)

  @stateful def withLog[T](log: PrintStream)(blk: => T)(implicit state: State): T = {
    if (config.verbosity >= 1) {
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
    if (config.verbosity >= 1) {
      val log = createLog(dir, filename)
      try {
        withLog(log)(blk)
      }
      finally {
        log.close()
      }
    }
    else blk
  }


  @stateful def log(x: => Any): Unit = if (config.verbosity >= 2) state.logstream.println(x)
  @stateful def dbg(x: => Any): Unit = if (config.verbosity >= 1) state.logstream.println(x)
  @stateful def msg(x: => Any, level: Int = 2): Unit = {
    state.logstream.println(x)
    if (config.verbosity >= level) System.out.println(x)
  }

  @stateful def report(x: => Any): Unit = if (config.verbosity >= 0) Report.report(x)

  @stateful def warn(x: => Any): Unit = if (config.showWarnings) Report.warn(x)
  @stateful def warn(ctx: SrcCtx): Unit = if (config.showWarnings) Report.warn(ctx)
  @stateful def warn(ctx: SrcCtx, showCaret: Boolean): Unit = if (config.showWarnings) Report.warn(ctx, showCaret)
  @stateful def warn(ctx: SrcCtx, x: => Any, noWarn: Boolean = false): Unit = {
    if (config.showWarnings) Report.warn(ctx, x)
    if (!noWarn && config.showWarnings) state.logWarning()
  }

  @stateful def error(x: => Any): Unit = if (config.showErrors) Report.error(x)
  @stateful def error(ctx: SrcCtx, showCaret: Boolean): Unit = if (config.showErrors) error(ctx, showCaret)
  @stateful def error(ctx: SrcCtx): Unit = if (config.showErrors) Report.error(ctx)
  @stateful def error(ctx: SrcCtx, x: => Any, noError: Boolean = false): Unit = {
    if (config.showErrors) Report.error(ctx, x)
    if (!noError) state.logError()
  }

  @stateful def bug(x: => Any): Unit = { state.logBug(); Report.bug(x) }
  @stateful def bug(ctx: SrcCtx): Unit = { state.logBug(); Report.bug(ctx) }
  @stateful def bug(ctx: SrcCtx, showCaret: Boolean): Unit = { state.logBug(); Report.bug(ctx, showCaret) }
  @stateful def bug(ctx: SrcCtx, x: => Any, noError: Boolean = false): Unit = { state.logBug(); Report.bug(ctx, x) }

  @stateful def info(x: => Any): Unit = Report.info(x)

  @stateful def str(lhs: Exp[_]): String = lhs match {
    case Def(rhs) => Report.readable(lhs) + " = " + Report.readable(rhs)
    case Const(c) => Report.readable(lhs) + " = " + Report.readable(c)
    case Param(p) => Report.readable(lhs) + " = " + Report.readable(p)
    case _: Bound[_] => Report.readable(lhs) + " [bound]"
  }
  @stateful def str(lhs: Seq[Exp[_]]): String = lhs.head match {
    case Def(rhs) => Report.readable(lhs) + " = " + Report.readable(rhs)
    case syms => Report.readable(syms)
  }

  @stateful def strMeta(lhs: Exp[_], tab: Int = 0) {
    lhs.name.foreach{name => dbg("  "*tab + c" - Name: $name") }
    dbg("  "*tab + c" - Type: ${lhs.tp}")
    metadata.get(lhs).foreach{m => dbg("  "*tab + c" - ${m._1}: ${m._2}") }
  }

  implicit def compilerReadable(sc: StringContext): Report.CompilerReportHelper = new Report.CompilerReportHelper(sc)
  implicit def frontendReadable(sc: StringContext): Report.FrontendReportHelper = new Report.FrontendReportHelper(sc)
}

