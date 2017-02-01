package argon.codegen.chiselgen

import argon.codegen.Codegen

trait ChiselCodegen extends Codegen {
  import IR._
  override val name = "Chisel Codegen"
  override val lang: String = "chisel"
  override val ext: String = "scala"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"${b.result}")
  }

  override def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case lhs: Sym[_] => s"x${lhs.id}"
  }


}
