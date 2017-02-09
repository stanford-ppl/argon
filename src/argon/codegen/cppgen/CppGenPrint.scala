package argon.codegen.cppgen

import argon.ops.PrintExp

trait CppGenPrint extends CppCodegen {
  val IR: PrintExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case Print(x)   => emit(src"""std::cout << $x;""")
    case Println(x) => emit(src"""std::cout << $x << std::endl;""")
    case _ => super.emitNode(lhs, rhs)
  }
}
