package argon.lang.direct

import argon.core._
import forge._

trait ArrayApi {
  implicit class NestedArrayInfixOps[T](a: MArray[MArray[T]]) {
    private implicit val mT = a.s.tp.typeArguments.head.typeArguments.head.asInstanceOf[Type[T]]
    @api def flatten: MArray[T] = a.flatMap{x => x}
  }
}
