package argon

import argon.codegen.FileGen
import scala.sys.process._

trait RunnerCore extends CompilerCore { self =>

  var testArgs = List[String]()

  def run(out: String): Int = {
    val start = System.currentTimeMillis()
    // msg("--------------------------")
    report(c"Executing ${Config.name}")
    //msg(c"in output directory $out")

    val proc = scala.sys.process.Process(Seq("sbt", s"""run ${testArgs
      .mkString(" ")}"""), new java.io.File(out))
    val logger = ProcessLogger { str =>
      msg(str)
    }
    val exitCode = withLog(Config.logDir, State.paddedPass + " RUN.log") {
      proc.run(logger).exitValue()
    }

    val time = (System.currentTimeMillis - start).toFloat
    if (exitCode != 0)
      error(s"Execution failed with non-zero exit code $exitCode")
    else
      report(s"[\u001B[32msuccess\u001B[0m] Execution completed")

    exitCode
  }

  override def compileOrRun(blk: => Unit): Unit = {
    super.compileOrRun(blk)
    passes.foreach {
      // TODO: More generic compilation / running
      case pass: FileGen if pass.lang == "scala" =>
        val exitCode = run(pass.out)
        if (exitCode != 0 && testbench) {
          throw new RunningFailed(exitCode)
        }

      case _ => // Do nothing
    }
  }
}
