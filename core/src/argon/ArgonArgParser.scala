package argon

import argon.util.ArgParser

import scopt._

class ArgonArgParser extends ArgParser {

  def scriptName = "argon"
  def description = "CLI for argon"
  //not sur yet if we must optional()

  /*parser.opt[String]('n', "name").action( (x,_) =>
    Config.name = x
  ).text("name of the app [app]")*/

  parser.opt[Unit]('q', "quiet").action( (_,_) =>
    Config.verbosity = 0
  ).text("disable background logging")

  parser.opt[Unit]('v', "verbose").action( (_,_) =>
    Config.verbosity = 2
  ).text("enable verbose printout")

  parser.opt[Unit]('c', "clean").action( (x,_) => {
      Config.clearGen = true
      Config.clearLogs = true
    }
  ).text("Reset output directory")

  parser.opt[Int]('m', "multifile").action( (x,_) =>
    Config.multifile = x
  ).text("""aggressiveness for splitting generated code files
      0 = no splitting or scoping
      1 = no splitting but yes scoping on inner pipes
      2 = no splitting but yes scoping everywhere
      3 <DEPRECATED> = splitting for inner pipes only
      4 = all blocks""")

  parser.opt[String]('o', "out").action( (x,_) =>
    Config.genDir = x
  ).text("location of output directory. Default is ./gen/<appname>")

  parser.opt[Int]('e', "emission").action( (x,_) =>
    Config.emitDevel = x
  ).text(
    """Conservativeness when emitting nodes.
      0 = crash when emitNode is undefined (release mode)
      1 = warn when undefined
      2 = warn when undefined and report when node matched but outside backend rules""")

  parser.opt[Int]('d', "detail").action( (x,_) =>
    Config.dotDetail = x
  ).text(
    """Amount of detail to emit when generating graphviz.
      false = control nodes only (legacy mode for PIR debugging)
      true = include info about primitives and retiming""")



}
