package argon.ops
import argon.core.Staging

trait VoidApi extends VoidExp {
  type Unit = Void
}

trait VoidExp extends Staging {
  case class Void(s: Exp[Void])

  /** Type classes **/
  // --- FStaged
  implicit object VoidType extends FStaged[Void] {
    override def unwrapped(x: Void) = x.s
    override def wrapped(x: Exp[Void]) = Void(x)
    override def typeArguments = Nil
    override def stagedClass = classOf[Void]
    override def isPrimitive = true
  }

  // --- Lift
  implicit object Unit2Void extends Lift[Unit,Void] { val FStaged = VoidType }

  /** Constant lifting **/
  implicit def unit2void(x: Unit)(implicit ctx: SrcCtx): Void = lift(x)
  def void()(implicit ctx: SrcCtx): Exp[Void] = constant[Void](())
}


