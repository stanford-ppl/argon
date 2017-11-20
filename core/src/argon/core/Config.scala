package argon.core

import com.typesafe.config.ConfigFactory
import pureconfig._

class Config {

  def sep = "/"
  var cwd: String = new java.io.File(".").getAbsolutePath

  var verbosity: Int = 0
  var showWarn: Boolean = true

  var unsafe: Boolean = false
  var lib:  Boolean = false
  var name: String = "App"
  var logDir: String = ""
  var genDir: String = ""
  var clearLogs: Boolean = true
  var clearGen: Boolean = true
  var multifile: Int = 4
  var enableNaming: Boolean = false
  var dotDetail: Int = 0
  var unwrapStructs: Boolean = true
  var emitDevel: Int = 0
  var allowAtomicWrites: Boolean = true

  var useAffine: Boolean = false

  def createInstance(): Config = new Config()

  def createClone(): Config = {
    val cfg = createInstance()
    cfg.verbosity = this.verbosity
    cfg.showWarn = this.showWarn
    cfg.unsafe = this.unsafe
    cfg.lib = this.lib
    cfg.name = this.name
    cfg.logDir = this.logDir
    cfg.genDir = this.genDir
    cfg.clearLogs = this.clearLogs
    cfg.clearGen = this.clearGen
    cfg.multifile = this.multifile
    cfg.enableNaming = this.enableNaming
    cfg.dotDetail = this.dotDetail
    cfg.unwrapStructs = this.unwrapStructs
    cfg.emitDevel = this.emitDevel
    cfg.allowAtomicWrites = this.allowAtomicWrites
    cfg.useAffine = this.useAffine
    cfg
  }

  def showWarnings: Boolean = showWarn && verbosity > - 1
  def showErrors: Boolean = verbosity > - 1

  //debugger interpreter
  var exit: () => Unit = () => ()

  def printer():String = {
    val vars = this.getClass.getDeclaredFields
    var cmd = ""
    for(v <- vars){
      try { 
        cmd = cmd + " --" + v.getName() + "=" + v.get(this)
      } catch {
        case e: java.lang.IllegalAccessException => 
          v.setAccessible(true)
          cmd = cmd + " --" + v.getName() + "=" + v.get(this)
          v.setAccessible(false)
        case _: Throwable => 
      }
    }
    cmd
  }

  case class ArgonConf(
    cwd: String,
    verbosity:Int,
    unsafe: Boolean,
    lib: Boolean,
    //name: String,
    log: String,
    out: String,
    clearLogs: Boolean,
    clearGen: Boolean,
    multifile: Int,
    detail: Int,
    unwrap: Boolean,
    emission: Int,
    atomicw: Boolean
  )

  def init() {

    val default = ConfigFactory.parseString(s"""
argon {
  cwd = $${user.dir}
  verbosity = 1
  unsafe = false
  lib = true
  log = $${argon.cwd}"/logs/"${name}
  out = $${argon.cwd}"/gen/"${name}
  clear-logs = true
  clear-gen = false
  multifile = 4
  detail = 0
  unwrap = true
  emission = 0
  atomicw = true
}
""")

    val mergedConf = ConfigFactory.load().withFallback(default).resolve()
    val conf = loadConfig[ArgonConf](mergedConf, "argon").right.get

    cwd = conf.cwd

    verbosity = conf.verbosity
    showWarn = true
    unsafe = conf.unsafe
    lib    = conf.lib
    //name = conf.name
    logDir = conf.log
    genDir = conf.out
    clearLogs = conf.clearLogs
    clearGen = conf.clearGen
    multifile = conf.multifile
    dotDetail = conf.detail
    unwrapStructs = conf.unwrap
    emitDevel = conf.emission// level of conservativeness and debug printing when emitting nodes
    allowAtomicWrites = conf.atomicw
  }
}
