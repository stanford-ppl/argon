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
      emit(src"$lhs[$i] = $i;")
      emitBlock(func)
      close("}")

    case ArrayForeach(array,apply,func,i) =>
      open(src"val $lhs = $array.indices.foreach{$i => ")
      visitBlock(apply)
      emitBlock(func)
      close("}")

    case ArrayMap(array,apply,func,i) =>
      emit(src"${lhs.tp}* $lhs = new ${lhs.tp}($array);")
      emit(src"for (int $i = 0; $i < $array; ${i}++) {")
      open(src"$array.indices.map{$i => ")
      emit(src"$array[$i] = $apply;")
      close("}")
      visitBlock(apply)
      emitBlock(func)

    case ArrayZip(a, b, applyA, applyB, func, i) =>
      Console.println(s"generic array $lhs ${lhs.tp}")
      emit(src"${lhs.tp}* $lhs = new ${lhs.tp}(${a}->length);")
      open(src"for (int $i = 0; $i < ${a}->length; ${i}++) { ")
      visitBlock(applyA)
      visitBlock(applyB)
      emitBlock(func)
      emit(src"${lhs}[$i] = ${func.result};")
      close("}")

      // 
    case ArrayReduce(array, apply, reduce, i, rV) =>
      emit(src"uint32_t $lhs = 0;")
      open(src"for (int $i = 0; $i < ${array}->length; ${i}++) {")
      emit(src"int32_t ${rV._1} = ${array}->apply($i);")
      emit(src"int32_t ${rV._2} = $lhs;")
      emit(src"$lhs = $lhs + ${array}->apply($i);")
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
