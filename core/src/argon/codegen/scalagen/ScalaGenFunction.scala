package argon.codegen.scalagen

import argon.core.Staging
import argon.ops.FunctionExp

trait ScalaGenFunction extends ScalaCodegen {
  val IR: FunctionExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case ArgonFunction1Type(t, r) => remap(t) + "=>" + remap(r)
    case _ => super.remap(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FunDecl(arg, block) =>
      open(src"def $lhs($arg: ${remap(arg.tp)}) = {")
      emitBlock(block)
      close("}")
    case Apply(fun, arg) =>
      emit(src"val $lhs = $fun($arg)")
    case _ => super.emitNode(lhs, rhs)
  }
}
