package argon.codegen.cppgen

import argon.ops.{ArrayExtExp, TextExp, FixPtExp, FltPtExp, BoolExp, IfThenElseExp, StructExp, TupleExp}

trait CppGenArrayExt extends CppGenArray {
  val IR: ArrayExtExp with TextExp with FixPtExp with FltPtExp with BoolExp with IfThenElseExp with StructExp with TupleExp
  import IR._

  private def getNestingLevel(tp: Staged[_]): Int = tp match {
    case tp: ArrayType[_] => 1 + getNestingLevel(tp.typeArguments.head) 
    case _ => 0
  }

  private def zeroElement(tp: Staged[_]): String = tp match {
    case tp: Tup2Type[_,_] => src"*(new ${tp}(0,0));"
    case _ => "0"
  }
 
  private def getPrimitiveType(tp: Staged[_]): String = tp match {
    case tp: ArrayType[_] => getPrimitiveType(tp.typeArguments.head) 
    case _ => remap(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case ArrayUpdate(array, i, data) => emit(src"${array}[$i] = $data;")
    case MapIndices(size, func, i)   =>
      emit(src"${lhs.tp}* $lhs = (${lhs.tp}*) std::malloc($size * sizeof(${lhs.tp}));")
      emit(src"int32_t ${lhs}_length = $size;")
      open(src"for (int $i = 0; $i < $size; ${i}++) {")
      emitBlock(func)
      emit(src"$lhs[$i] = ${func.result};")
      close("}")

    case ArrayForeach(array,apply,func,i) =>
      open(src"val $lhs = $array.indices.foreach{$i => ")
      visitBlock(apply)
      emitBlock(func)
      close("}")

    case ArrayMap(array,apply,func,i) =>
      emit(src"${lhs.tp}* $lhs = (${lhs.tp}*) std::malloc(${array}_length * sizeof(${array.tp}));")
      emit(src"int32_t ${lhs}_length = ${array}_length;")
      open(src"for (int $i = 0; $i < ${array}_length; $i++) { ")
      visitBlock(apply)
      emitBlock(func)
      emit(src"$lhs[$i] = ${func.result};")
      close("}")
      

    case ArrayZip(a, b, applyA, applyB, func, i) =>
      emit(src"${lhs.tp}* $lhs = (${lhs.tp}*) std::malloc(${a}_length * sizeof(${a.tp}));")
      emit(src"int32_t ${lhs}_length = ${a}_length;")
      open(src"for (int $i = 0; $i < ${a}_length; ${i}++) { ")
      visitBlock(applyA)
      visitBlock(applyB)
      emitBlock(func)
      emit(src"${lhs}[$i] = ${func.result};")
      close("}")

      // 
    case ArrayReduce(array, apply, reduce, i, rV) =>

      if (isArrayType(lhs.tp)) {
        emit(src"""${lhs.tp}${if (isArrayType(lhs.tp)) "*" else ""} $lhs;""") 
      } else {
        emit(src"${lhs.tp} $lhs;") 
      }
      open(src"if (${array}_length > 0) { // Hack to handle reductions on things of length 0")
      emit(src"$lhs = ${array}[0];")
      closeopen("} else {")
      emit(src"$lhs = ${zeroElement(lhs.tp)};")
      close("}")
      open(src"for (int $i = 1; $i < ${array}_length; ${i}++) {")
      emit(src"""${rV._1.tp}${if (isArrayType(rV._1.tp)) "*" else ""} ${rV._1} = ${array}[$i];""")
      emit(src"""${rV._2.tp}${if (isArrayType(rV._2.tp)) "*" else ""} ${rV._2} = $lhs;""")
      emitBlock(reduce)
      emit(src"$lhs = ${reduce.result};")
      close("}")

    case ArrayFilter(array, apply, cond, i) =>
      open(src"val $lhs = $array.filter{${apply.result} => ")
      emitBlock(cond)
      close("}")

    case ArrayFlatMap(array, apply, func, i) =>
      val nesting = getNestingLevel(array.tp)
      emit("// TODO: flatMap node assumes the func block contains only applies (.flatten)")

      // Initialize lhs array
      (0 until nesting).map{ level => 
        val grabbers = (0 until level).map{ m => """->apply(0))""" }.mkString("")
        val openParens = (0 until level).map{ m => "(" }.mkString{""}
        emit(src"int size_${lhs}_$level = ${openParens}${array}${grabbers}_length;")
      }
      emit(src"""${lhs.tp}* $lhs = new ${lhs.tp}(${(0 until nesting).map{ m => src"size_${lhs}_$m" }.mkString("*")});""")

      // Open all levels of loop
      (0 until nesting).foreach { level => 
        val grabbers = (0 until level).map{ "[0]" }.mkString("") 
        open(src"for (int ${i}_$level = 0; ${i}_${level} < size_${lhs}_$level; ${i}_${level}++) { ")
      }

      // Pluck off elements of the $array
      val applyString = (0 until nesting).map{ level => src"""->apply(${i}_${level}))""" }.mkString("")
      val parensString = (0 until nesting).map{ level => """(""" }.mkString("")
      emit(src"${getPrimitiveType(lhs.tp)} ${func.result} = ${parensString}${array}${applyString};")

      // Update the lhs
      val flatIndex = (0 until nesting).map{ level => 
        src"""${ (level+1 until nesting).map{ k => src"size_${lhs}_$k" }.mkString("*") } ${ if (level+1 < nesting) "*" else "" }${i}_${level}"""
      }.mkString(" + ")
      emit(src"$lhs[$flatIndex] = ${func.result};")

      // Close all levels of loop
      (0 until nesting).foreach { level => 
        val grabbers = (0 until level).map{ "[0]" }.mkString("") 
        close("}")
      }


    case _ => super.emitNode(lhs, rhs)
  }
}
