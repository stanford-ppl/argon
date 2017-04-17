package argon.codegen.scalagen

import argon.ops.BoolExp

trait ScalaGenBool extends ScalaCodegen {
  val IR: BoolExp
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case BoolType => "Boolean"
    case _        => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: Boolean) => c.toString
    case _                 => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)     => emit(src"val $lhs = !$x")
    case And(x, y)  => emit(src"val $lhs = $x && $y")
    case Or(x, y)   => emit(src"val $lhs = $x || $y")
    case XOr(x, y)  => emit(src"val $lhs = $x != $y")
    case XNor(x, y) => emit(src"val $lhs = $x == $y")
    case RandomBool(None) =>
      emit(
        src"val $lhs = java.util.concurrent.ThreadLocalRandom.current().nextBoolean()")
    case RandomBool(Some(max)) =>
      emit(
        src"val $lhs = java.util.concurrent.ThreadLocalRandom.current().nextBoolean() && $max")
    case StringToBool(x) => emit(src"val $lhs = $x.toBoolean")
    case _               => super.emitNode(lhs, rhs)
  }
}
