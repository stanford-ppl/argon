package argon.ops
import argon.core.Staging

trait PrintApi extends PrintExp with TextApi with VoidApi {
  def println()(implicit ctx: SrcCtx): Void = println("")
}

trait PrintExp extends Staging with TextExp with VoidExp {
  /** Direct methods **/
  def print[T:Staged](x: T)(implicit ctx: SrcCtx): Void = Void(misc_print(textify(x).s))
  def println[T:Staged](x: T)(implicit ctx: SrcCtx): Void = Void(misc_println(textify(x).s))

  def print(x: String)(implicit ctx: SrcCtx): Void = print(string2text(x))
  def println(x: String)(implicit ctx: SrcCtx): Void = println(string2text(x))

  /** IR Nodes **/
  case class Print(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = misc_print(f(x)) }
  case class Println(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = misc_println(f(x)) }

  /** Smart Constructors **/
  def misc_print(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Void] = stageSimple(Print(x))(ctx)
  def misc_println(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Void] = stageSimple(Println(x))(ctx)
}


