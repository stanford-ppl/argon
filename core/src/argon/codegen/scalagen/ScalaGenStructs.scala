package argon.codegen.scalagen

import argon.core._
import argon.codegen.StructCodegen
import argon.nodes._

trait ScalaGenStructs extends ScalaCodegen with StructCodegen {

  protected def structName(tp: StructType[_], idx: Int): String = s"Struct$idx"

  protected def emitStructDeclaration(name: String, tp: StructType[_]): Unit = {
    open(src"case class $name(")
    val fields = tp.fields.map{case (field, t) => src"var $field: $t"}.mkString(",\n" + tabbed)
    emit(fields)
    close(") {")
    open("")
    emit("override def productPrefix = \"" + tp.prefix + "\"")
    close("}")
  }

  protected def emitDataStructures(): Unit = if (encounteredStructs.nonEmpty) {
    withStream(newStream("Structs")) {
      for ((tp, name) <- encounteredStructs) {
        emitStructDeclaration(name, tp)
        emit("")
      }
    }
  }

  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (st: StructType[_], e@Const(elems)) =>
      val seq = elems.asInstanceOf[Seq[(_, Exp[_])]]
      src"new ${st}( " + seq.map(x => quote(x._2)).mkString(", ") + " )"

    case _ => super.quoteConst(c)
  }
  

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case e: StructAlloc[_] =>
      emit(src"val $lhs: ${e.mR} = new ${e.mR}( " + e.elems.map(x => quote(x._2)).mkString(", ") + " )")

    case FieldUpdate(struct, field, value) => emit(src"val $lhs = $struct.$field = $value")
    case FieldApply(struct, field)         => emit(src"val $lhs = $struct.$field")

    case _ => super.emitNode(lhs, rhs)
  }


}
