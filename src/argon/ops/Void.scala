package argon.ops
import argon.core.{Base,Staging}

trait Voids extends Base {
  type Void
  implicit object Unit2Void extends Lift[Unit,Void] { val staged = VoidType }
  implicit def unit2void(x: Unit): Void = lift(x)
  implicit val VoidType: Staged[Void]
}
trait VoidApi extends Voids {
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

  def void(x: Unit): Sym[Void] = const[Void](x)
}


