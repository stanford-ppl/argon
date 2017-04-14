package argon.codegen.cppgen

import argon.core.Staging
import argon.ops.AssertExp

trait CppGenAsserts extends CppCodegen {
  val IR: AssertExp with Staging
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Assert(cond, m)       =>  emit(src"assert($cond);")
    case _ => super.emitNode(lhs, rhs)
  }
}
