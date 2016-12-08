package argon

object Config {
  private def getProperty(prop: String, default: String) = {
    val p1 = System.getProperty(prop)
    val p2 = System.getProperty(prop.substring(1))
    if (p1 != null && p2 != null) {
      assert(p1 == p2, "ERROR: conflicting properties")
      p1
    }
    else if (p1 != null) p1 else if (p2 != null) p2 else default
  }

  val cwd = System.getProperty("user.dir")

  var verbosity: Int = getProperty("argon.verbosity", "2").toInt
  var genDir: String = getProperty("argon.out", s"$cwd/gen")
  var logDir: String = getProperty("argon.logs", s"$cwd/logs")
  var unsafe: Boolean = getProperty("argon.unsafe", "false").toBoolean
  var lib:  Boolean = getProperty("argon.lib", "false").toBoolean
  var name: String = getProperty("argon.name", "app")
  var clearLogs: Boolean = getProperty("argon.clearLogs", "false").toBoolean
  var unwrapStructs: Boolean = getProperty("argon.unwrap", "true").toBoolean
}
