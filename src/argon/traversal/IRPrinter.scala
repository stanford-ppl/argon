package argon.traversal

import argon.core.Staging
import argon.utils.escapeConst

// Print IR + metadata for each encountered symbol
trait IRPrinter extends Traversal {
  val IR: Staging
  import IR._

  override val name = "PrinterPlus"
  override def shouldRun = verbosity > 0

  def strMeta(lhs: Exp[_]) {
    debugs(c" - Type: ${lhs.tp}")
    metadata.get(lhs).foreach{m => if (null == m) c" - ${m._1}: NULL" else debugs(c" - ${m._1}: ${m._2}") }
  }

  override protected def visit(lhs: Sym[_], rhs: Op[_]) = {
    if (rhs.blocks.nonEmpty)
      msgs(c"$lhs = $rhs {")
    else
      msgs(c"$lhs = $rhs")
    strMeta(lhs)

    rhs.blocks.zipWithIndex.foreach{case (blk,i) =>
      tab += 1
      msgs(c"block $i: $blk {")
      tab += 1
      visitBlock(blk)
      tab -= 1
      msgs(c"} // End of $lhs block #$i")
      tab -= 1
    }
    if (rhs.blocks.nonEmpty) msgs(c"} // End of $lhs")
  }
}
