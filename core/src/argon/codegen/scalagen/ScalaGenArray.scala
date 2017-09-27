package argon.codegen.scalagen

import argon.core._
import argon.nodes._

trait ScalaGenArray extends ScalaCodegen {
  override protected def remap(tp: Type[_]): String = tp match {
    case tp: ArrayType[_] => src"Array[${tp.typeArguments.head}]"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    // Array constants are currently disallowed
    case _ => super.quoteConst(c)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@ArrayNew(size)      => emit(src"val $lhs = new Array[${op.mA}]($size)")
    case op@ArrayFromSeq(seq)   => emit(src"""val $lhs = Array[${op.mA}](${seq.map(quote).mkString(",")})""")

    case ArrayApply(array, i)   => emit(src"val $lhs = $array.apply($i)")
    case ArrayLength(array)     => emit(src"val $lhs = $array.length")
    case InputArguments()       => emit(src"val $lhs = args")

    case ArrayUpdate(array, i, data) => emit(src"val $lhs = $array.update($i, $data)")
    case MapIndices(size, func, i)   =>
      open(src"val $lhs = Array.tabulate($size){$i => ")
      emitBlock(func)
      close("}")

    case ArrayForeach(array,apply,func,i) =>
      open(src"val $lhs = $array.indices.foreach{$i => ")
      visitBlock(apply)
      emitBlock(func)
      close("}")

    case ArrayMap(array,apply,func,i) =>
      open(src"val $lhs = Array.tabulate($array.length){$i => ")
      visitBlock(apply)
      emitBlock(func)
      close("}")

    case ArrayZip(a, b, applyA, applyB, func, i) =>
      open(src"val $lhs = Array.tabulate($a.length){$i => ")
      visitBlock(applyA)
      visitBlock(applyB)
      emitBlock(func)
      close("}")

    case ArrayReduce(array, apply, reduce, i, rV) =>
      open(src"val $lhs = $array.reduce{(${rV._1},${rV._2}) => ")
      emitBlock(reduce)
      close("}")

    case ArrayFilter(array, apply, cond, i) =>
      open(src"val $lhs = $array.filter{${apply.result} => ")
      emitBlock(cond)
      close("}")

    case ArrayFlatMap(array, apply, func, i) =>
      open(src"val $lhs = $array.flatMap{${apply.result} => ")
      emitBlock(func)
      close("}")

    case _ => super.emitNode(lhs, rhs)
  }
}
