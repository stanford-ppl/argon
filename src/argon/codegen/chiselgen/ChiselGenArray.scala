package argon.codegen.chiselgen

import argon.ops.ArrayExp

trait ChiselGenArray extends ChiselCodegen {
  val IR: ArrayExp
  import IR._

  override protected def remap(tp: Staged[_]): String = tp match {
    case tp: ArrayType[_] => src"List[${tp.typeArguments.head}]"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    // Array constants are currently disallowed
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => emit(src"val $lhs = new Array[${op.mA}]($size)")
    case ArrayApply(array, i)   => emit(src"val $lhs = ${array}($i)")
    // case ArrayUpdate(array,i,e) => emit(src"val $lhs = $array.update($i, $e)")
    case ArrayLength(array)     => emit(src"val $lhs = $array.length")
    case InputArguments()       => emit(src"val $lhs = args")
    case _ => super.emitNode(lhs, rhs)
  }
}
