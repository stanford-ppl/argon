package argon.traversal

import argon.core.Statements

trait BlockTraversal {
  val IR: Statements
  import IR._

  protected var innerScope: Seq[Int] = _

  private def availableStms = if (innerScope ne null) innerScope else 0 until IR.curNodeId

  private def withInnerScope[A](scope: Seq[Int])(body: => A): A = {
    val saveInner = innerScope
    innerScope = scope
    val result = body
    innerScope = saveInner
    result
  }

  private def scopeSanityCheck[T:Staged](block: Block[T], scope: Seq[Int]): Unit = {
    val observable = block.effects.map(defOf).map(_.id).distinct // node ids for effect producers
    val actual = observable intersect scope
    val missing = observable diff actual
    if (missing.nonEmpty) {
      val expectedStms = observable.map{s => stmFromNodeId(s)}
      val actualStms = actual.map{s => stmFromNodeId(s)}
      val missingStms = missing.map{s => stmFromNodeId(s)}
      throw new EffectsOrderException(block.getResult, expectedStms, actualStms, missingStms)
    }
  }

  final def traverseBlock(block: Block[_]): Unit = __traverseBlock(block)(mstg(block.tp))
  final def __traverseBlock[T:Staged](block: Block[T]): Unit = {
    val join = block.getResult +: block.effects
    val schedule = IR.getLocalSchedule(availableNodes = availableStms, result = join.map(_.id))

    scopeSanityCheck(block, schedule)

    // Delay all other symbols as part of the inner scope
    withInnerScope(availableStms diff schedule) {
      schedule.foreach { nodeId =>
        val stm = stmFromNodeId(nodeId)
        visitStm(stm)
      }
    }
  }

  def visitStm(stm: Stm): Unit = stm.rhs.blocks.foreach {blk => traverseBlock(blk) }
}
