package argon.codegen.scalagen

import argon.codegen.StructCodegen
import argon.ops.StructExp

trait ScalaGenStructs extends ScalaCodegen with StructCodegen {
  val IR: StructExp
  import IR._

  def structName(tp: StructType[_], idx: Int): String = s"Struct$idx"

  def emitStructDeclaration(name: String, tp: StructType[_]): Unit = {
    open(src"case class $name(")
    val fields = tp.fields.map{case (field: String, t: Staged[_]) => src"var $field: $t"}.mkString(",\n")
    emit(fields)
    close(")")
  }

  def emitDataStructures(): Unit = if (encounteredStructs.nonEmpty) {
    withStream(newStream("Structs")) {
      for ((tp, name) <- encounteredStructs) {
        emitStructDeclaration(name, tp)
        emit("")
      }
    }
  }


  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case e: StructAlloc[_] =>
      emit(src"val $lhs: ${e.mR} = new ${e.mR}( " + e.elems.map(x => quote(x._2)).mkString(", ") + " )")

    case FieldUpdate(struct, field, value) => emit(src"val $lhs = $struct.$field = $value")
    case FieldApply(struct, field)         => emit(src"val $lhs = $struct.$field")

    case _ => super.emitNode(lhs, rhs)
  }


}
