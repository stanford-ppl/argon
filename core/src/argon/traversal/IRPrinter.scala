package argon.traversal

// Print IR + metadata for each encountered symbol
trait IRPrinter extends Traversal {
  import IR._

  override val name = "PrinterPlus"
  override def shouldRun = verbosity >= 1

  def strMeta(lhs: Exp[_]) {
    nameOf(lhs).foreach{name => dbgs(c" - Name: $name") }
    dbgs(c" - Type: ${lhs.tp}")
    metadata.get(lhs).foreach{m => dbgs(c" - ${m._1}: ${m._2}") }
  }

  def printBlocks(lhs: Sym[_], blocks: Seq[Block[_]]) = blocks.zipWithIndex.foreach{case (blk,i) =>
    tab += 1
    dbgs(c"block $i: $blk {")
    tab += 1
    logs(c"effects: ${blk.summary}")
    logs(c"anti-deps: ${blk.effectful}")
    visitBlock(blk)
    tab -= 1
    dbgs(c"} // End of $lhs block #$i")
    tab -= 1
  }

  override protected def visit(lhs: Sym[_], rhs: Op[_]) = {
    if (rhs.blocks.nonEmpty)
      dbgs(c"$lhs = $rhs {")
    else
      dbgs(c"$lhs = $rhs")
    strMeta(lhs)

    printBlocks(lhs, rhs.blocks)

    if (rhs.blocks.nonEmpty) dbgs(c"} // End of $lhs")
  }

  override def visitFat(lhs: Seq[Sym[_]], rhs: Def) = {
    if (rhs.blocks.nonEmpty)
      dbgs(c"$lhs = $rhs {")
    else
      dbgs(c"$lhs = $rhs")
    strMeta(lhs.head)
    lhs.foreach{s => val Def(d) = s; assert(d == rhs) }

    printBlocks(lhs.head, rhs.blocks)

    if (rhs.blocks.nonEmpty) dbgs(c"} // End of $lhs")
  }
}
