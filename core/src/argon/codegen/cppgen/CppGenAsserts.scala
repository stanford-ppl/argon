package argon.codegen.cppgen

import argon.core._
import argon.nodes._

trait CppGenAsserts extends CppCodegen {

  override protected def remap(tp: Type[_]): String = tp match {
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = c match {
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Assert(cond, m)       =>  
      val msg = src"""${m.getOrElse("API Assert Failed")}"""
      emit(src"""ASSERT($cond, "\n=================\n${msg.replace("string(\"","").replace("\")","")}\n=================\n");""")
    case _ => super.emitNode(lhs, rhs)
  }
}
