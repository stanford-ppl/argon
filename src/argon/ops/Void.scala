package argon.ops

import argon.core.Core
import scala.language.implicitConversions

trait VoidCore extends Core {
  case class Void() extends Sym { self =>
    override type LibType = Unit
    def tp = VoidType.asInstanceOf[Typ[self.type]]
  }

  implicit object VoidType extends Typ[Void] {
    override def next = Void()
    override def typeArguments = Nil
    override def stagedClass = classOf[Void]
    override def isPrimitive = true
  }

  implicit def unit_to_void(x: Unit): Void = fresh[Void].asConst(())
}

trait VoidAPI extends VoidCore {
  type Unit = Void
}
