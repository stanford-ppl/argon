package argon.util
import argon.core.Reporting
import scopt._

trait ArgParser extends Reporting {
  def description: String
  def scriptName: String

  val parser = new scopt.OptionParser[Unit](scriptName) {}

  parser.note(description +"\n")

  def parse(args: Seq[String]) = parser.parse(args, ())

}
