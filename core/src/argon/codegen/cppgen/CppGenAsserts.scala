package argon.codegen.cppgen

import argon._
import argon.nodes._

trait CppGenAsserts extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[?]): String = c match {
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Assert(cond, m)       =>  emit(src"""ASSERT($cond, "\n=================\n${m.getOrElse("API Assert Failed")}\n=================\n");""")
    case _ => super.emitNode(lhs, rhs)
  }
}
