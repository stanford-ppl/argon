package argon.ops
import argon.core.Staging
import org.virtualized.stageany

trait VoidApi extends VoidExp {
  self: TextExp =>
  type Unit = Void
}

@stageany
trait VoidExp extends Staging {
  self: TextExp =>
  case class Void(s: Exp[Void]) extends StageAny[Void] {
    def ===(that: Void)(implicit ctx: SrcCtx) = ???
    def =!=(that: Void)(implicit ctx: SrcCtx) = ???

    override def toText(implicit ctx: SrcCtx) = textify(this)
  }

  /** Type classes **/
  // --- Staged
  implicit object VoidType extends FStaged[Void] {
    override def wrapped(x: Exp[Void]) = Void(x)
    override def typeArguments = Nil
    override def stagedClass = classOf[Void]
    override def isPrimitive = true
  }

  // --- Lift
  implicit object Unit2Void extends Lift[Unit,Void] {
    override def lift(x: Unit)(implicit ctx: SrcCtx) = Void(void())
  }

  /** Constant lifting **/
  implicit def unit2void(x: Unit)(implicit ctx: SrcCtx): Void = lift(x)
  def void()(implicit ctx: SrcCtx): Exp[Void] = constant[Void](())
}


