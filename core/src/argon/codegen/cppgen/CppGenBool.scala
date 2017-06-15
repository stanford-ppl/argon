package argon.codegen.cppgen

import argon.internals._
import argon.nodes._

trait CppGenBool extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case BooleanType => "bool"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case Const(c: Boolean) => c.toString
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)       => emit(src"bool $lhs = !$x;")
    case And(x,y)     => emit(src"bool $lhs = $x && $y;")
    case Or(x,y)      => emit(src"bool $lhs = $x || $y;")
    case XOr(x,y)     => emit(src"bool $lhs = $x != $y;")
    case XNor(x,y)    => emit(src"bool $lhs = $x == $y;")
    case RandomBoolean(x) => emit(src"bool $lhs = java.util.concurrent.ThreadLocalRandom.current().nextBoolean();")
    case StringToBoolean(x) => emit(src"bool $lhs = $x.toBoolean")
    case _ => super.emitNode(lhs, rhs)
  }
}
