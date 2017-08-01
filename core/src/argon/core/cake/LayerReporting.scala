package argon.core.cake

import argon.util._
import forge._

trait LayerReporting { self: ArgonCake =>
  type Log = java.io.PrintStream

  def plural(x: Int, singular: String, plur: String): String = if (x == 1) singular else plur

  def createLog(dir: String, filename: String): Log = Report.createLog(dir, filename)
  @stateful def withLog[T](log: Log)(blk: => T): T = Report.withLog(log)(blk)
  @stateful def withLog[T](dir: String, filename: String)(blk: => T): T = Report.withLog(dir, filename)(blk)

  @stateful def withConsole[T](blk: => T): T = Report.withLog(System.out)(blk)

  @stateful def log(x: => Any): Unit = Report.log(x)
  @stateful def dbg(x: => Any): Unit = Report.dbg(x)
  @stateful def msg(x: => Any, level: Int = 2): Unit = Report.msg(x, level)

  def report(x: => Any): Unit = Report.report(x)

  def warn(x: => Any): Unit = Report.warn(x)
  def warn(ctx: SrcCtx, showCaret: Boolean): Unit = Report.warn(ctx, showCaret)
  def warn(ctx: SrcCtx): Unit = Report.warn(ctx)
  @stateful def warn(ctx: SrcCtx, x: => Any, noWarn: Boolean = false): Unit = Report.warn(ctx, x, noWarn)

  def error(x: => Any): Unit = Report.error(x)
  def error(ctx: SrcCtx, showCaret: Boolean): Unit = Report.error(ctx, showCaret)
  def error(ctx: SrcCtx): Unit = Report.error(ctx)
  @stateful def error(ctx: SrcCtx, x: => Any, noError: Boolean = false): Unit = Report.error(ctx, x, noError)

  def bug(x: => Any): Unit = Report.bug(x)
  def bug(ctx: SrcCtx, showCaret: Boolean): Unit = Report.bug(ctx, showCaret)
  def bug(ctx: SrcCtx): Unit = Report.bug(ctx)
  @stateful def bug(ctx: SrcCtx, x: => Any, noError: Boolean = false): Unit = Report.bug(ctx, x, noError)

  def info(x: => Any): Unit = Report.info(x)

  @stateful def str(lhs: Exp[_]): String = Report.str(lhs)
  @stateful def str(lhs: Seq[Exp[_]]): String = Report.str(lhs)
  @stateful def strMeta(lhs: Exp[_], tab: Int = 0) {
    lhs.name.foreach{name => dbg("  "*tab + c" - Name: $name") }
    dbg("  "*tab + c" - Type: ${lhs.tp}")
    metadata.get(lhs).foreach{m => dbg("  "*tab + c" - ${m._1}: ${m._2}") }
  }

  implicit def compilerReadable(sc: StringContext): Report.CompilerReportHelper = new Report.CompilerReportHelper(sc)
  implicit def frontendReadable(sc: StringContext): Report.FrontendReportHelper = new Report.FrontendReportHelper(sc)
}

