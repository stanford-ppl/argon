package argon.codegen.cppgen

import argon.ops.PrintExp

trait CppGenPrint extends CppCodegen {
  val IR: PrintExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case Print(x)   => emit(s"""std::cout << $x;""")
    case Println(x) => emit(s"""std::cout << $x << std::endl;""")
    case _ => super.emitNode(lhs, rhs)
  }
}
