package argon.codegen.scalagen

import argon._
import argon.nodes._

trait ScalaGenBoolean extends ScalaCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case BooleanType => "Boolean"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: Boolean) => c.toString
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)    => emit(src"val $lhs = !$x")
    case And(x,y)  => emit(src"val $lhs = $x && $y")
    case Or(x,y)   => emit(src"val $lhs = $x || $y")
    case XOr(x,y)  => emit(src"val $lhs = $x != $y")
    case XNor(x,y) => emit(src"val $lhs = $x == $y")
    case RandomBoolean(None) => emit(src"val $lhs = java.util.concurrent.ThreadLocalRandom.current().nextBoolean()")
    case RandomBoolean(Some(max)) => emit(src"val $lhs = java.util.concurrent.ThreadLocalRandom.current().nextBoolean() && $max")
    case StringToBoolean(x) => emit(src"val $lhs = $x.toBoolean")
    case _ => super.emitNode(lhs, rhs)
  }
}
