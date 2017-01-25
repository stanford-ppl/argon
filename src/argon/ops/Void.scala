package argon.ops
import argon.core.{Base,Staging}

trait VoidOps extends Base {
  type Void
  implicit object Unit2Void extends Lift[Unit,Void] { val staged = VoidType }
  implicit def unit2void(x: Unit)(implicit ctx: SrcCtx): Void = lift(x)
  implicit val VoidType: Staged[Void]
}
trait VoidApi extends VoidOps {
  type Unit = Void
}

trait VoidExp extends VoidOps with Staging {
  case class Void(s: Exp[Void])
  def void()(implicit ctx: SrcCtx): Exp[Void] = constant[Void](())

  implicit object VoidType extends Staged[Void] {
    override def unwrapped(x: Void) = x.s
    override def wrapped(x: Exp[Void]) = Void(x)
    override def typeArguments = Nil
    override def stagedClass = classOf[Void]
    override def isPrimitive = true
  }
}


