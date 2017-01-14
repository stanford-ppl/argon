package argon.ops
import argon.core.Base

trait PrintOps extends Base with TextOps with VoidOps {
  def print[S:Staged](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println[S:Staged](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println()(implicit ctx: SrcCtx): Void = println("")

  def print(x: String)(implicit ctx: SrcCtx): Void = print(string2text(x))
  def println(x: String)(implicit ctx: SrcCtx): Void = println(string2text(x))

  def print(x: Text)(implicit ctx: SrcCtx): Void
  def println(x: Text)(implicit ctx: SrcCtx): Void
}
trait PrintApi extends PrintOps with TextApi with VoidApi


trait PrintExp extends PrintOps with TextExp with VoidExp {
  /** API **/
  def print(x: Text)(implicit ctx: SrcCtx): Void = Void(misc_print(x.s))
  def println(x: Text)(implicit ctx: SrcCtx): Void = Void(misc_println(x.s))


  /** IR Nodes **/
  case class Print(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = misc_print(f(x)) }
  case class Println(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = misc_println(f(x)) }


  /** Smart Constructors **/
  def misc_print(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Void] = stageSimple(Print(x))(ctx)
  def misc_println(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Void] = stageSimple(Println(x))(ctx)
}
