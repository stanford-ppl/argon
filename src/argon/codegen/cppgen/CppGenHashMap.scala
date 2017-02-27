package argon.codegen.cppgen

import argon.ops.HashMapExp


trait CppGenHashMap extends CppCodegen {
  val IR: HashMapExp
  import IR._

  // override protected def remap(tp: Staged[_]): String = tp match {
  //   case HashIndexType(mK) => src"scala.collection.mutable.HashMap[$mK,${typ[Index]}]"
  //   case _ => super.remap(tp)
  // }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case HashIndexApply(index, key) => 
      emit(src"${lhs.tp} $lhs = -1;")
      open(src"for (int ${lhs}_i; ${lhs}_i < ${index}->length; ${lhs}_i++) {")
      emit(src"if (${index}->apply(${lhs}_i) == ${key}) { $lhs = ${lhs}_i; }")
      close("}")
    case _ => super.emitNode(lhs, rhs)
  }

  override protected def emitFat(lhs: List[Sym[_]], rhs: Def) = rhs match {
    case e @ ArgonBuildHashMap(in, apply, keyFunc, valFunc, reduce, rV, i) =>
      emit(src"cppDeliteArray${e.mK}* ${quote(lhs(0))} = new cppDeliteArray${e.mK}(${in}->length); // Keys")
      emit(src"cppDeliteArray${e.mV}* ${lhs(1)} = new cppDeliteArray${e.mV}(${in}->length); // Values")
      emit(src"cppDeliteArray${e.mK}* ${lhs(2)} = ${lhs(0)}; // TODO: Probably totally wrong.  lhs2 appears to be the scala hashmap, but it seems like we only use it to lookup index of a key")
      emit(s"long size = 0;")
      //emit(src"//val ${lhs(2)}  = new ${HashIndexType(e.mK)}()")
      open(src"for (int $i = 0; $i < ${in}->length; $i++) { ")
        emit(s"// Apply block")
        visitBlock(apply)
        emit(s"// Key block")
        visitBlock(keyFunc)
        emit(s"// Val block")
        visitBlock(valFunc)
        emit(s"bool contained = false;")
        emit(s"long idx = -1;")
        emit(src"for (int i = 0; i < size; i++) {if (${lhs(0)}->apply(i) == ${keyFunc.result}) {contained = true;}}")
        open(src"if (contained) {")
          emit(src"${rV._1.tp}* ${rV._1} = ${valFunc.result};")
          emit(src"${rV._2.tp}* ${rV._2} = ${lhs(1)}->apply(idx);")
          visitBlock(reduce)
          emit(src"${lhs(1)}->update(idx, ${reduce.result});")
        closeopen("} else {")
          emit(s"size += 1;")
          emit(src"//index += ${keyFunc.result} -> ${lhs(1)}.size")
          emit(src"${lhs(0)}->update(size, ${keyFunc.result});")
          emit(src"${lhs(1)}->update(size, ${valFunc.result});")
        close("}")
      close("}")

    case _ => super.emitFat(lhs, rhs)
  }

}
