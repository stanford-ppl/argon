package argon.codegen.scalagen

import argon.core._
import argon.nodes._
import argon.emul.FixedPoint

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

    case ArrayApply(array, i)   => 
      emit(src"val $lhs = $array.apply($i)")
      array match {
        case Def(InputArguments()) => 
          if (lhs.name.isDefined) {
            val ii = i match {case c: Const[_] => c match {case Const(c: FixedPoint) => c.toInt; case _ => -1}; case _ => -1}
            if (cliArgs.contains(ii)) cliArgs += (ii -> s"${cliArgs(ii)} / ${lhs.name.get}")
            else cliArgs += (ii -> lhs.name.get)
          }
        case _ =>
      }

    case ArrayLength(array)     => emit(src"val $lhs = $array.length")
    case InputArguments()       => 
      emit(src"val $lhs = args")
      emit(src"""if (args.contains("--help") || args.contains("-h")) {printHelp()}""")

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
