package argon.util

import argon.core._

import virtualized.{SourceContext => SrcCtx}

object Report {
  def plural(x: Int, sing: String, plur: String): String = if (x == 1) sing else plur

  def report(x: => Any): Unit = System.out.println(x)

  def warn(x: => Any): Unit = System.out.println(s"[\u001B[33mwarn\u001B[0m] $x")
  def warn(ctx: SrcCtx, x: => Any): Unit = warn(ctx.toString + ": " + x)
  def warn(ctx: SrcCtx): Unit = warn(ctx, showCaret = false)
  def warn(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    warn(ctx.lineContent.get)
    if (showCaret) warn(" "*(ctx.column-1) + "^") else warn("")
  }

  def error(x: => Any): Unit = System.out.println(s"[\u001B[31merror\u001B[0m] $x")
  def error(ctx: SrcCtx, x: => Any): Unit = error(ctx.fileName + ":" + ctx.line + ": " + x)
  def error(ctx: SrcCtx): Unit = error(ctx, showCaret = false)
  def error(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    error(ctx.lineContent.get)
    if (showCaret) error(" "*(ctx.column-1) + "^") else error("")
  }

  def bug(x: => Any): Unit = System.out.println(s"[\u001B[35mbug\u001B[0m] $x")
  def bug(ctx: SrcCtx, x: => Any): Unit = bug(ctx.fileName + ":" + ctx.line + ": " + x)
  def bug(ctx: SrcCtx): Unit = bug(ctx, showCaret = false)
  def bug(ctx: SrcCtx, showCaret: Boolean): Unit = if (ctx.lineContent.isDefined) {
    bug(ctx.lineContent.get)
    if (showCaret) bug(" "*(ctx.column-1) + "^") else bug("")
  }

  def info(x: => Any): Unit = System.out.println(s"[\u001B[34minfo\u001B[0m] $x")

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