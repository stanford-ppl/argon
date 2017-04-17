package argon.util
import argon.core.Reporting
import scopt._

trait ArgParser extends Reporting {
  def description: String
  def scriptName: String

  val parser = new scopt.OptionParser[Unit](scriptName) {}

  parser.head(scriptName, description)
  parser.help("help").text("prints this usage text")
  
  def parse(args: Seq[String]) =
    parser.parse(args, ())
  

}
