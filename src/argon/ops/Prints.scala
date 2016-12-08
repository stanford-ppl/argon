package argon.ops
import argon.core.Base

trait Prints extends Base with Texts with Voids {
  def print[S:Staged](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println[S:Staged](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println()(implicit ctx: SrcCtx): Void = println("")

  def print(x: String)(implicit ctx: SrcCtx): Void = print(lift(x))
  def println(x: String)(implicit ctx: SrcCtx): Void = println(lift(x))

  def print(x: Text)(implicit ctx: SrcCtx): Void
  def println(x: Text)(implicit ctx: SrcCtx): Void
}
trait PrintAPI extends Prints with TextAPI with VoidAPI


trait PrintExp extends Prints with TextExp with VoidExp {
  /** IR Nodes **/
  case class Print(x: Sym[Text]) extends Op[Void] { def mirror(f:Tx) = print(f(x)) }
  case class Println(x: Sym[Text]) extends Op[Void] { def mirror(f:Tx) = println(f(x)) }

  /** Evaluation **/
  // eval[Print]{case Print(Const(s: String)) => System.out.print(s) }
  // eval[Println]{case Println(Const(s: String)) => System.out.println(s) }

  def println(x: Text)(implicit ctx: SrcCtx): Void = stageSimple(Println(x.s))(ctx)
  def print(x: Text)(implicit ctx: SrcCtx): Void = stageSimple(Print(x.s))(ctx)
}
