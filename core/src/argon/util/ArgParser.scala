package argon.util
import argon.core.Reporting
import scopt._

class ArgParser(val scriptName: String) extends Reporting {
  val MAX_COLS = 80
  val description = ""

  val parser = new scopt.OptionParser[Unit](scriptName) {}



  def parse(args: Seq[String]) = parser.parse(args, ())

}
