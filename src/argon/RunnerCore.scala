package argon

import argon.codegen.SingleFileGen
import scala.sys.process._

trait RunnerCore extends CompilerCore {
  self =>

  def run(out: String): Int = {
    val start = System.currentTimeMillis()
    msg("--------------------------")
    msg(c"Running ${self.getClass}")
    //msg(c"in output directory $out")

    val proc = scala.sys.process.Process(Seq("sbt", "run"), new java.io.File(out))
    val output = proc.run()
    val exitCode = output.exitValue()

    val time = (System.currentTimeMillis - start).toFloat
    msg(s"Completed in " + "%.4f".format(time/1000) + " seconds")

    exitCode
  }

  override def main(sargs: Array[String]): Unit = {
    super.main(sargs)
    passes.foreach {
      // TODO: More generic compilation / running
      case pass: SingleFileGen if pass.lang == "scala" =>
        val exitCode = run(pass.out)
        if (exitCode != 0 && testbench) throw new RunningFailed(exitCode)

      case _ => // Do nothing
    }


  }
}
