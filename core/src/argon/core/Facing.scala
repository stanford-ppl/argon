package argon.core

trait FrontendFacing {
  def toStringFrontend: String
}

trait CompilerFacing {
  def toStringCompiler: String
}
