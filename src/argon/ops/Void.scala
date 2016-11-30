package argon.ops

import argon.core.{Base,Core}
import scala.language.implicitConversions

trait VoidOps extends Base {
  type Void
  implicit def lift(x: Unit): Void
  implicit val VoidType: Typ[Void]
}
trait VoidAPI extends VoidOps {
  type Unit = Void
}

trait VoidCore extends VoidOps with Core {
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

  implicit def lift(x: Unit): Void = fresh[Void].asConst(())
}


