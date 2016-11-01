package argon.core

trait BlockTraversal {
  val IR: Statements
  import IR._

  protected var innerScope: Seq[Int] = _

  private def availableStms = if (innerScope ne null) innerScope else 0 until IR.graph.curNodeId

  private def withInnerScope[A](scope: Seq[Int])(body: => A): A = {
    val saveInner = innerScope
    innerScope = scope
    val result = body
    innerScope = saveInner
    result
  }

  private def scopeSanityCheck[S:Typ](block: Block[S], scope: Seq[Int]): Unit = {
    val observable = block.effects.flatMap(defOf).map(_.id).distinct // node ids for effect producers
    val actual = observable intersect scope
    val missing = observable diff actual
    if (missing.nonEmpty) {
      val expectedStms = observable.map{s => stmFromNodeId(s)}
      val actualStms = actual.map{s => stmFromNodeId(s)}
      val missingStms = missing.map{s => stmFromNodeId(s)}
      throw new EffectsOrderException(block.getResult, expectedStms, actualStms, missingStms)
    }
  }

  final def traverseBlock(block: Block[_]): Unit = __traverseBlock(block)(mtyp(block.tp))
  final def __traverseBlock[T:Typ](block: Block[T]): Unit = {
    val res  = block.getResult match {case s: Sym => Some(s); case _ => None}
    val join = block.effects ++ res
    val schedule = IR.graph.getLocalSchedule(availableNodes = availableStms, result = join.map(_.id))

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
