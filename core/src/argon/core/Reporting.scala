package argon.core

import argon._
import forge._
import java.io.PrintStream
import java.nio.file.{Files, Paths}

// TODO: Move these two elsewhere?
trait UserFacing {
  def toStringUser: String
}
trait CompilerFacing {
  def toStringCompiler: String
}


trait Reporting {
  def plural(x: Int, sing: String, plur: String): String = if (x == 1) sing else plur

  def createLog(dir: String, filename: String): PrintStream = {
    Files.createDirectories(Paths.get(dir))
    new PrintStream(dir + Config.sep + filename)
  }

  @stateful final def withLog[T](log: PrintStream)(blk: => T): T = {
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

  @stateful final def withLog[T](dir: String, filename: String)(blk: => T): T = {
    val log = createLog(dir, filename)
    try {
      withLog(log)(blk)
    }
    finally {
      log.close()
    }
  }
  final def withConsole[T](blk: => T): T = withLog(System.out)(blk)

  // TODO: Should these be macros?
  @stateful final def log(x: => Any): Unit = if (Config.verbosity >= 2) state.logstream.println(x)
  @stateful final def dbg(x: => Any): Unit = if (Config.verbosity >= 1) state.logstream.println(x)
  final def msg(x: => Any, level: Int = 2)(implicit state: State): Unit = {
    state.logstream.println(x)
    if (Config.verbosity >= level) System.out.println(x)
  }

  final def report(x: => Any): Unit = if (Config.verbosity >= 0) System.out.println(x)
  final def warn(x: => Any): Unit = if (Config.showWarn) {
    System.err.println(s"[\u001B[33mwarn\u001B[0m] $x")
    dbg(s"[warn] $x")
  }
  final def error(x: => Any): Unit = {
    System.err.println(s"[\u001B[31merror\u001B[0m] $x")
    dbg(s"[error] $x")
  }

  @stateful final def warn(ctx: SrcCtx, x: => Any, noWarn: Boolean = false): Unit = {
    warn(ctx.toString() + ": " + x)
    if (!noWarn) state.logWarning()
  }
  @stateful final def error(ctx: SrcCtx, x: => Any, noError: Boolean = false): Unit = {
    error(ctx.fileName + ":" + ctx.line + ": " + x)
    if (!noError) state.logError()
  }

  final def warn(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    warn(ctx.lineContent.get)
    if (showCaret) warn(" "*(ctx.column-1) + "^") else warn("")
  }
  final def error(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    error(ctx.lineContent.get)
    if (showCaret) error(" "*(ctx.column-1) + "^") else error("")
  }

  final def warn(ctx: SrcCtx): Unit = warn(ctx, showCaret = false)
  final def error(ctx: SrcCtx): Unit = error(ctx, showCaret = false)

  final def str(lhs: Exp[_]): String = lhs match {
    case Def(rhs) => c"$lhs = $rhs"
    case Const(c) => c"$lhs = $c"
    case _: Bound[_] => c"$lhs [bound]"
  }
  final def str(lhs: Seq[Exp[_]]): String = lhs.head match {
    case Def(rhs) => c"$lhs = $rhs"
    case syms => c"$syms"
  }

  private def readable(x: Any): String = x match {
    case c: CompilerFacing => c.toStringCompiler
    case c:Class[_]        => c.getName.split('$').last.replace("class ", "")
    case p:Iterable[_]     => if (p.isEmpty) "Nil" else p.map(readable).mkString("Seq(", ",", ")")
    case p:Product         => if (p.productIterator.isEmpty) c"${p.productPrefix}"
                              else c"""${p.productPrefix}(${p.productIterator.map(readable).mkString(", ")})"""
    case _ =>
      if (x == null) "null" else x.toString
  }

  private def userReadable(x: Any): String = x match {
    case c: UserFacing => c.toStringUser
    case c:Class[_]  => c.getName.split('$').last.replace("class ", "")
    case _ => readable(x)
  }

  implicit class CompileReportHelper(sc: StringContext) {
    def c(args: Any*): String = sc.raw(args.map(readable): _*).stripMargin
  }
  implicit class FrontendReportHelper(sc: StringContext) {
    def u(args: Any*): String = sc.raw(args.map(userReadable): _*).stripMargin
  }

}
