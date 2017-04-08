package argon

import argon.util.ArgParser

import scopt._

class ArgonArgParser extends ArgParser("argon") {
  //not sur yet if we must optional()
  parser.opt[Unit]('q', "quiet").action( (_,_) =>
    Config.verbosity = 0
  ).text("disable background logging")

  parser.opt[Unit]('v', "verbose").action( (_,_) =>
    Config.verbosity = 2
  ).text("enable verbose printout")

  parser.opt[Unit]('c', "clean").action( (x,_) =>
    Config.clearGen = true
  ).text("Reset output directory")


  parser.opt[Int]('m', "multifile").action( (x,_) =>
    Config.multifile = x
  ).text("aggressiveness for splitting generated code files\n0 = no splitting or scoping\n1 = no splitting but yes scoping on inner pipes\n2 = no splitting but yes scoping everywhere\n3 <DEPRECATED> = splitting for inner pipes only\n4 = all blocks")

  parser.opt[String]('o', "out").action( (x,_) =>
    Config.genDir = x
  ).text("location of output directory. Default is ./gen/<appname>")
  
}
