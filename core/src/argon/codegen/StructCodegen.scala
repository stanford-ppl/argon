package argon.codegen

import argon.ops.StructExp
import scala.collection.mutable

trait StructCodegen extends Codegen {
  val IR: StructExp
  import IR._

  val encounteredStructs = mutable.HashMap[StructType[_], String]()
  var structNumber: Int = 0

  override protected def remap(tp: Type[_]) = tp match {
    case t: StructType[_] =>
      encounteredStructs.getOrElseUpdate(t, { structNumber += 1; structName(t, structNumber) })

    case _ => super.remap(tp)
  }

  protected def structName(tp: StructType[_], idx: Int): String
  protected def emitDataStructures(): Unit

  override protected def postprocess[S: Type](block: Block[S]) = {
    emitDataStructures()
    super.postprocess(block)
  }


}
