package argon.ops
import argon.core.Staging

trait VoidApi extends VoidExp {
  self: TextExp =>
  type Unit = Void
}

trait VoidExp extends Staging {
  self: TextExp =>

  case class Void(s: Exp[Void]) extends MetaAny[Void] {
    def ===(that: Void)(implicit ctx: SrcCtx) = true
    def =!=(that: Void)(implicit ctx: SrcCtx) = false
    override def toText(implicit ctx: SrcCtx) = textify(this)
  }

  /** Type classes **/
  // --- Staged
  implicit object VoidType extends Meta[Void] {
    override def wrapped(x: Exp[Void]) = Void(x)
    override def stagedClass = classOf[Void]
    override def isPrimitive = true
  }

  // --- Lift
  implicit object Unit2Void extends Lift[Unit,Void] {
    val staged = typ[Void]
    override def apply(x: Unit)(implicit ctx: SrcCtx) = Void(void())
  }

  /** Constant lifting **/
  implicit def unit2void(x: Unit)(implicit ctx: SrcCtx): Void = lift(x)
  def void()(implicit ctx: SrcCtx): Exp[Void] = constant[Void](())
}


