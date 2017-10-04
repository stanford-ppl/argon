package argon.traversal

import argon.core._

// Print IR + metadata for each encountered symbol
case class IRPrinter(var IR: State) extends Traversal {
  override val name = "IR Printout"
  override def shouldRun = verbosity >= 1

  def strMeta(lhs: Exp[_]) {
    lhs.name.foreach{name => dbgs(c" - Name: $name") }
    if (lhs.prevNames.nonEmpty) {
      val aliases = lhs.prevNames.map{case (tx,alias) => s"$tx: $alias" }.mkString(", ")
      dbgs(c" - Aliases: $aliases")
    }
    dbgs(c" - Type: ${lhs.tp}")
    metadata.get(lhs).foreach{m => dbgs(c" - ${m._1}: ${m._2}") }
  }

  def printBlocks(lhs: Sym[_], blocks: Seq[Block[_]]) = blocks.zipWithIndex.foreach{case (blk,i) =>
    tab += 1
    dbgs(c"block $i: $blk {")
    tab += 1
    dbgs(c"effects: ${blk.effects}")
    dbgs(c"anti-deps: ${blk.effectful}")
    dbgs(c"isolated: ${blk.isolated}")
    dbgs(c"seal: ${blk.seal}")
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
