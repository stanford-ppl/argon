package argon.lang.direct

import argon.core._
import argon.nodes._
import forge._

trait StructExp {
  @internal def struct[T:StructType](fields: (CString, Exp[_])*): T = Struct[T](fields:_*)
}
