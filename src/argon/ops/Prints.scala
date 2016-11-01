package argon.ops


trait PrintCore extends TextCore with VoidCore {
  /** IR Nodes **/
  case class Print(x: Text) extends Op[Void] { def mirror(f:Tx) = misc_print(f(x)) }
  case class Println(x: Text) extends Op[Void] { def mirror(f:Tx) = misc_println(f(x)) }

  /** Evaluation **/
  eval[Print]{case Print(Const(s: String)) => System.out.print(s) }
  eval[Println]{case Println(Const(s: String)) => System.out.println(s) }

  /** Internal methods **/
  def misc_println(x: Text)(implicit ctx: SrcCtx): Void = stageSimple(Print(x))(ctx)
  def misc_print(x: Text)(implicit ctx: SrcCtx): Void = stageSimple(Println(x))(ctx)
}

trait PrintAPI extends PrintCore {
  def println[S:Typ](x: S)(implicit ctx: SrcCtx): Void = misc_println(textify(x))
  def println(x: Text)(implicit ctx: SrcCtx): Void = misc_println(x)(ctx)
  def println()(implicit ctx: SrcCtx): Void = misc_println(lift(""))(ctx)
}
