package argon

import argon.core.Config
import argon.util.ArgParser
import scopt._

class ArgonArgParser(config: Config) extends ArgParser {

  def scriptName = "argon"
  def description = "CLI for argon"
  //not sur yet if we must optional()

  /*parser.opt[String]('n', "name").action( (x,_) =>
    config.name = x
  ).text("name of the app [app]")*/

  // Note: this is now the default
  parser.opt[Unit]('q', "quiet").action( (_,_) =>
    config.verbosity = 0
  ).text("disable background logging")

  parser.opt[Unit]('v', "verbose").action( (_,_) =>
    config.verbosity = 1
  ).text("enable verbose printout")

  parser.opt[Unit]("vv").action( (_,_) =>
    config.verbosity = 2
  ).text("enable verbose printout")


  parser.opt[Int]("verbosity").action( (x,_) =>
    config.verbosity = x
  ).text("set verbosity level")
  
  parser.opt[Unit]('c', "clean").action( (x,_) => {
      config.clearGen = true
      config.clearLogs = true
    }
  ).text("Reset output directory")

  parser.opt[Int]('m', "multifile").action( (x,_) =>
    config.multifile = x
  ).text("""aggressiveness for splitting generated code files
      0 = no splitting or scoping
      1 = no splitting but yes scoping on inner pipes
      2 = no splitting but yes scoping everywhere
      3 <DEPRECATED> = splitting for inner pipes only
      4 = all blocks""")

  parser.opt[String]('o', "out").action( (x,_) =>
    config.genDir = x
  ).text("location of output directory. Default is ./gen/<appname>")

  parser.opt[Int]('e', "emission").action( (x,_) =>
    config.emitDevel = x
  ).text(
    """Conservativeness when emitting nodes.
      0 = crash when emitNode is undefined (release mode)
      1 = warn when undefined
      2 = warn when undefined and report when node matched but outside backend rules""")

  parser.opt[Int]('d', "detail").action( (x,_) =>
    config.dotDetail = x
  ).text(
    """Amount of detail to emit when generating graphviz.
      0 = control nodes only (legacy mode for PIR debugging)
      1 = include info about primitives and retiming""")



}
