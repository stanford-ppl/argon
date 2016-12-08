package argon.ops
import argon.core.{Base,Staging}

trait Voids extends Base {
  type Void
  implicit def lift(x: Unit): Void
  implicit val VoidType: Staged[Void]
}
trait VoidAPI extends Voids {
  type Unit = Void
}

trait VoidExp extends Voids with Staging {
  case class Void(s: Sym[Void])

  implicit object VoidType extends Staged[Void] {
    override def unwrap(x: Void) = x.s
    override def wrap(x: Sym[Void]) = Void(x)
    override def typeArguments = Nil
    override def stagedClass = classOf[Void]
    override def isPrimitive = true
  }

  implicit def lift(x: Unit): Void = liftConst[Void](x)
}


