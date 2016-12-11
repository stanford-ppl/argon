package argon.ops
import argon.core.Base

trait Prints extends Base with Texts with Voids {
  def print[S:Staged](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println[S:Staged](x: S)(implicit ctx: SrcCtx): Void = println(textify(x))
  def println()(implicit ctx: SrcCtx): Void = println("")

  def print(x: String)(implicit ctx: SrcCtx): Void = print(string2text(x))
  def println(x: String)(implicit ctx: SrcCtx): Void = println(string2text(x))

  def print(x: Text)(implicit ctx: SrcCtx): Void
  def println(x: Text)(implicit ctx: SrcCtx): Void
}
trait PrintApi extends Prints with TextApi with VoidApi


trait PrintExp extends Prints with TextExp with VoidExp {
  /** API **/
  def print(x: Text)(implicit ctx: SrcCtx): Void = Void(misc_print(x.s))
  def println(x: Text)(implicit ctx: SrcCtx): Void = Void(misc_println(x.s))


  /** IR Nodes **/
  case class Print(x: Sym[Text]) extends Op[Void] { def mirror(f:Tx) = misc_print(f(x)) }
  case class Println(x: Sym[Text]) extends Op[Void] { def mirror(f:Tx) = misc_println(f(x)) }


  /** Smart Constructors **/
  def misc_print(x: Sym[Text])(implicit ctx: SrcCtx): Sym[Void] = stageSimple(Print(x))(ctx)
  def misc_println(x: Sym[Text])(implicit ctx: SrcCtx): Sym[Void] = stageSimple(Println(x))(ctx)
}
