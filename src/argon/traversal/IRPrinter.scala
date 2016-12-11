package argon.traversal

import argon.core.Staging
import argon.utils.{escapeChar, escapeString}

// Print IR + metadata for each encountered symbol
trait IRPrinter extends Traversal {
  val IR: Staging
  import IR._

  override val name = "PrinterPlus"

  def strMeta(lhs: Sym[_]) {
    debugs(c" - Type: ${lhs.tp}")
    metadata.get(lhs).foreach{m => if (null == m) c" - ${m._1}: NULL" else debugs(c" - ${m._1}: ${m._2}") }
  }

  override def visit(lhs: Sym[_], rhs: Op[_]) = {
    rhs.inputs.foreach{
      case sym@Const(c: String) =>
        msgs(c"$sym = ${escapeString(c)}")
        strMeta(sym)
      case sym@Const(c: Char) =>
        msgs(c"$sym = ${escapeChar(c)}")
        strMeta(sym)
      case sym@Const(c) =>
        msgs(c"$sym = $c")
        strMeta(sym)
      case _ =>
    }

    if (rhs.blocks.nonEmpty)
      msgs(c"$lhs = $rhs {")
    else
      msgs(c"$lhs = $rhs")
    strMeta(lhs)

    rhs.blocks.zipWithIndex.foreach{case (blk,i) =>
      tab += 1
      msgs(c"block $i: $blk {")
      tab += 1
      traverseBlock(blk)
      tab -= 1
      msgs(c"} // End of $lhs block #$i")
      tab -= 1
    }
    if (rhs.blocks.nonEmpty) msgs(c"} // End of $lhs")
  }

  override def visitBlock[S:Staged](b: Block[S]) = {
    msgs(c"Scheduling $b")
    super.visitBlock(b)
  }

  // Only run traversal if debugging/verbose mode is enabled
  override def run[S:Staged](b: Block[S]) = if (verbosity > 0) super.run(b) else b
}
