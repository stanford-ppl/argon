package argon.ops

import argon._
import forge._

trait PrintApi extends PrintExp { self: ArgonApi =>
  /** Direct methods **/
  @api def println(): Void = println("")

  @api def print[T:Meta](x: T): Void = Void(misc_print(textify(x).s))
  @api def println[T:Meta](x: T): Void = Void(misc_println(textify(x).s))

  @api def print(x: java.lang.String): Void = print(string2text(x))
  @api def println(x: java.lang.String): Void = println(string2text(x))
}

trait PrintExp { self: ArgonExp =>
  /** IR Nodes **/
  case class Print(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = misc_print(f(x)) }
  case class Println(x: Exp[Text]) extends Op[Void] { def mirror(f:Tx) = misc_println(f(x)) }

  /** Smart Constructors **/
  def misc_print(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Void] = stageSimple(Print(x))(ctx)
  def misc_println(x: Exp[Text])(implicit ctx: SrcCtx): Exp[Void] = stageSimple(Println(x))(ctx)
}


