package argon.codegen.scalagen

import argon.ops.ArrayExp

trait ScalaGenArray extends ScalaCodegen {
  val IR: ArrayExp
  import IR._

  override protected def remap(tp: Staged[_]): String = tp match {
    case tp: ArrayType[_] => src"Array[${tp.typeArguments.head}]"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    // Array constants are currently disallowed
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size) => src"val $lhs = new Array[${op.mA}]($size)"
    case ArrayApply(array, i) => src"val $lhs = $array.apply($i)"
    case ArrayUpdate(array,i,e) => src"val $lhs = $array.update($i, $e)"
    case _ => super.emitNode(lhs, rhs)
  }
}
