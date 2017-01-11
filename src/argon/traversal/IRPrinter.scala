package argon.traversal

import argon.core.Staging
import argon.utils.escapeConst

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
      case sym@Const(c) =>
        msgs(c"$sym = ${escapeConst(c)} : ${c.getClass}")
        strMeta(sym)
      case _ =>
    }

    if (rhs.scopes.nonEmpty)
      msgs(c"$lhs = $rhs {")
    else
      msgs(c"$lhs = $rhs")
    strMeta(lhs)

    rhs.scopes.zipWithIndex.foreach{case (blk,i) =>
      tab += 1
      msgs(c"block $i: $blk {")
      tab += 1
      traverseScope(blk)
      tab -= 1
      msgs(c"} // End of $lhs block #$i")
      tab -= 1
    }
    if (rhs.scopes.nonEmpty) msgs(c"} // End of $lhs")
  }

  override def visitScope[S:Staged](b: Scope[S]) = {
    msgs(c"Scheduling $b")
    super.visitScope(b)
  }

  // Only run traversal if debugging/verbose mode is enabled
  override def run[S:Staged](b: Scope[S]) = if (verbosity > 0) super.run(b) else b
}
