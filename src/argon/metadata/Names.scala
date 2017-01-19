package argon.metadata

import argon.core._

trait NameOps extends Base
trait NameApi extends NameOps
trait NameExp extends NameOps with Staging {
  object nameOf {
    def apply(x: Exp[_]): Option[String] = ctxsOf(x).headOption.flatMap(_.assignedVariable)
  }
}
