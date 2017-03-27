package argon.codegen.chiselgen

import argon.ops.HashMapExp


trait ChiselGenHashMap extends ChiselCodegen {
  val IR: HashMapExp
  import IR._

  override protected def remap(tp: Type[_]): String = tp match {
    case HashIndexType(mK) => src"scala.collection.mutable.HashMap[$mK,${typ[Index]}]"
    case _ => super.remap(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case HashIndexApply(index, key) => emit(src"val $lhs = $index.getOrElse($key, -1)")
    case _ => super.emitNode(lhs, rhs)
  }

  override protected def emitFat(lhs: List[Sym[_]], rhs: Def) = rhs match {
    case e @ ArgonBuildHashMap(in, apply, keyFunc, valFunc, reduce, rV, i) =>

    case _ => super.emitFat(lhs, rhs)
  }

}
