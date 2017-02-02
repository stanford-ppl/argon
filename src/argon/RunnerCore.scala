package argon

import argon.codegen.FileGen
import scala.sys.process._

trait RunnerCore extends CompilerCore {
  self =>

  var testArgs = List[String]()

  def run(out: String): Int = {
    val start = System.currentTimeMillis()
    // msg("--------------------------")
    msg(c"Running ${Config.name}")
    //msg(c"in output directory $out")

    val proc = scala.sys.process.Process(Seq("sbt", s"""run ${testArgs.mkString(" ")}"""), new java.io.File(out))
    val output = proc.run()
    val exitCode = output.exitValue()

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed in " + "%.4f".format(time/1000) + " seconds")

    exitCode
  }

  override def compileOrRun(blk: => Unit): Unit = {
    super.compileOrRun(blk)
    passes.foreach {
      // TODO: More generic compilation / running
      case pass: FileGen if pass.lang == "scala" =>
        val exitCode = run(pass.out)
        if (exitCode != 0 && testbench) throw new RunningFailed(exitCode)

      case _ => // Do nothing
    }
  }
}