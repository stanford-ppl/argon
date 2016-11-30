package argon.ops
import argon.core.Base
import virtualized.SourceContext  // Why is this necessary?

trait PrintOps extends Base with TextOps with VoidOps {
  def print[S:Typ](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println[S:Typ](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println()(implicit ctx: SrcCtx): Void = println("")

  def print(x: String)(implicit ctx: SrcCtx): Void = print(lift(x))
  def println(x: String)(implicit ctx: SrcCtx): Void = println(lift(x))

  def print(x: Text)(implicit ctx: SrcCtx): Void
  def println(x: Text)(implicit ctx: SrcCtx): Void
}
trait PrintAPI extends PrintOps


trait PrintCore extends PrintOps with TextCore with VoidCore {
  /** IR Nodes **/
  case class Print(x: Text) extends Op[Void] {
    def mirror(f:Tx) = print(f(x))
  }
  case class Println(x: Text) extends Op[Void] { def mirror(f:Tx) = println(f(x)) }

  /** Evaluation **/
  eval[Print]{case Print(Const(s: String)) => System.out.print(s) }
  eval[Println]{case Println(Const(s: String)) => System.out.println(s) }

  def println(x: Text)(implicit ctx: SrcCtx): Void = stageSimple(Println(x))(ctx)
  def print(x: Text)(implicit ctx: SrcCtx): Void = stageSimple(Print(x))(ctx)
}
