package argon.codegen.cppgen

import argon.ops.{ArrayExtExp, TextExp, FixPtExp, FltPtExp, BoolExp}

trait CppGenArrayExt extends CppGenArray {
  val IR: ArrayExtExp with TextExp with FixPtExp with FltPtExp with BoolExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ArrayUpdate(array, i, data) => emit(src"val $lhs = $array.update($i, $data)")
    case MapIndices(size, func, i)   =>
      emit(src"${lhs.tp}* $lhs = new ${lhs.tp}($size);")
      open(src"for (int $i = 0; $i < $size; ${i}++) {")
      emitBlock(func)
      emit(src"$lhs->update($i, ${func.result});")
      close("}")

    case ArrayForeach(array,apply,func,i) =>
      open(src"val $lhs = $array.indices.foreach{$i => ")
      visitBlock(apply)
      emitBlock(func)
      close("}")

    case ArrayMap(array,apply,func,i) =>
      emit(src"${lhs.tp}* $lhs = new ${lhs.tp}($array);")
      open(src"for (int $i = 0; $i < ${array}->length; $i++) { ")
      emitBlock(func)
      emit(src"$lhs->update($i, ${func.result});")
      close("}")
      visitBlock(apply)

    case ArrayZip(a, b, applyA, applyB, func, i) =>
      emit(src"${lhs.tp}* $lhs = new ${lhs.tp}(${a}->length);")
      open(src"for (int $i = 0; $i < ${a}->length; ${i}++) { ")
      visitBlock(applyA)
      visitBlock(applyB)
      emitBlock(func)
      emit(src"${lhs}->update($i, ${func.result});")
      close("}")

      // 
    case ArrayReduce(array, apply, reduce, i, rV) =>
      emit(src"${lhs.tp} $lhs;")
      open(src"if (${array}->length > 0) { // Hack to handle reductions on things of length 0")
      emit(src"$lhs = ${array}->apply(0);")
      closeopen("} else {")
      emit(src"$lhs = 0;")
      close("}")
      open(src"for (int $i = 1; $i < ${array}->length; ${i}++) {")
      emit(src"${rV._1.tp} ${rV._1} = ${array}->apply($i);")
      emit(src"${rV._2.tp} ${rV._2} = $lhs;")
      emitBlock(reduce)
      emit(src"$lhs = ${reduce.result};")
      close("}")

    case ArrayFilter(array, apply, cond, i) =>
      open(src"val $lhs = $array.filter{${apply.result} => ")
      emitBlock(cond)
      close("}")

    case ArrayFlatMap(array, apply, func, i) =>
      open(src"val $lhs = $array.indices.flatMap{$i => ")
      visitBlock(apply)
      emitBlock(func)
      close("}")

    case _ => super.emitNode(lhs, rhs)
  }
}
