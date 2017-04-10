package argon

import com.typesafe.config.ConfigFactory
import pureconfig._

object Config {

  val default = ConfigFactory.parseString("""
argon {
  cwd = ${user.dir}
  verbosity = 1
  unsafe = false
  lib = true
  name = "app"
  log = ${argon.cwd}"/logs/"${argon.name}
  out = ${argon.cwd}"/gen/"${argon.name}
  clear-logs = true
  clear-gen = false
  multifile = 4
  unwrap = true
  emission = 0
  atomicw = true
}
""")

  val mergedConf = ConfigFactory.load().withFallback(default).resolve()

  case class ArgonConf(
    cwd: String,
    verbosity:Int,
    unsafe: Boolean,
    lib: Boolean,
    name: String,
    log: String,
    out: String,
    clearLogs: Boolean,
    clearGen: Boolean,
    multifile: Int,
    unwrap: Boolean,
    emission: Int,
    atomicw: Boolean
  )

  val conf = loadConfig[ArgonConf](mergedConf, "argon").right.get

  def sep = "/"
  var cwd = conf.cwd

  var verbosity: Int = conf.verbosity
  var showWarn: Boolean = true

  var unsafe: Boolean = conf.unsafe
  var lib:  Boolean = conf.lib
  var name: String = conf.name
  var logDir: String = conf.log
  var genDir: String = conf.out
  var clearLogs: Boolean = conf.clearLogs
  var clearGen: Boolean = conf.clearGen
  var multifile: Int = conf.multifile
  var unwrapStructs: Boolean = conf.unwrap
  var emitDevel: Int = conf.emission// level of conservativeness and debug printing when emitting nodes
  var allowAtomicWrites: Boolean = conf.atomicw
}
