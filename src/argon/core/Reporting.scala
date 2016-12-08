package argon.core

import argon.Config
import java.io.{File, PrintStream}
import scala.virtualized.SourceContext

trait Reporting {
  private var logstream: PrintStream = System.out

  final def withLog[T](dir: String, filename: String)(blk: => T): T = {
    val save = logstream
    new File(dir).mkdirs()
    logstream = new PrintStream(new File(s"$dir" + java.io.File.separator + s"${Config.name} $filename"))
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
  final def debug(x: => Any): Unit = if (Config.verbosity >= 2) logstream.println(x)
  final def msg(x: => Any): Unit = if (Config.verbosity >= 1) {
    logstream.println(x)
    if (logstream != System.out) System.out.println(x)
  }
  final def report(x: => Any): Unit = if (Config.verbosity >= 0) System.out.println(x)
  final def warn(x: => Any): Unit = System.err.println(s"[\u001B[33mwarn\u001B[0m] $x")
  final def error(x: => Any): Unit = System.err.println(s"[\u001B[31merror\u001B[0m] $x")

  final def warn(ctx: SourceContext, x: => Any): Unit = warn(ctx.toString() + ": " + x)
  final def error(ctx: SourceContext, x: => Any): Unit = error(ctx.toString() + ": " + x)

  def readable(x: Any): String = x match {
    //case s:Sym         => "x"+s.id
    case c:Class[_]    => c.getName.split('$').last.replace("class ", "")

    case p:Iterable[_] => if (p.isEmpty) "Nil" else p.map(readable).mkString("(", ", ", ")")
    case p:Product     => if (p.productIterator.isEmpty) c"${p.productPrefix}"
                          else c"${p.productPrefix}${p.productIterator.toList}"
    case _ => x.toString
  }

  implicit class ReportHelper(sc: StringContext) {
    def c(args: Any*): String = {
      sc.raw(args.map(readable): _*).stripMargin
    }
  }

}
