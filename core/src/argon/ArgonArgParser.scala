package argon

import argon.util.ArgParser

class ArgonArgParser extends ArgParser("argon") {
  addArg(Switch('q', "disable background logging"){Config.verbosity = 0})
  addArg(Switch('v', "enable verbose printout"){Config.verbosity = 2})

  addArg(LongSwitch("clean", "Reset output directory"){Config.clearGen = true})

  addArg(Named("emission", "conservativeness when emitting nodes.\n0 = crash on undefined generation rule (release mode)\n1 = warn when undefined,\n2 = warn when undefined and report when node matched but is outside backend rules"){
    arg => Config.emitDevel = arg.toInt})
  addArg(Named("multifile", "aggressiveness for splitting generated code files\n0 = no splitting or scoping\n1 = no splitting but yes scoping on inner pipes\n2 = no splitting but yes scoping everywhere\n3 <DEPRECATED> = splitting for inner pipes only\n4 = all blocks"){
    arg => Config.multifile = arg.toInt})
  addArg(Named("outdir", "location of output directory. Default is ./gen/<appname>"){
    arg => Config.genDir = arg})

}
