package argon.codegen.chiselgen

import argon.core.Staging
import argon.ops.BoolExp

trait ChiselGenBool extends ChiselCodegen {
  val IR: BoolExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case BoolType => "Boolean"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: Boolean) => c.toString + ".B"
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)       => alphaconv_register(src"$lhs"); emit(src"val $lhs = !$x")
    case And(x,y)     => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x && $y")
    case Or(x,y)      => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x || $y")
    case XOr(x,y)     => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x =/= $y")
    case XNor(x,y)    => alphaconv_register(src"$lhs"); emit(src"val $lhs = $x === $y")
    case RandomBool(x) => emit(src"val $lhs = java.util.concurrent.ThreadLocalRandom.current().nextBoolean()")
    case StringToBool(x) => emit(src"val $lhs = $x.toBoolean")
    case _ => super.emitNode(lhs, rhs)
  }
}
