package argon.traversal

import argon.core.Staging

trait BlockTraversal {
  val IR: Staging
  import IR._

  private var innerScope: Seq[Int] = _

  final protected def scrubSym(s: Sym[_]): Unit = {
    IR.scrubSymbol(s)
    innerScope = innerScope.filterNot(_ == s.id)
  }

  private def availableStms = if (innerScope ne null) innerScope else 0 until IR.curNodeId

  // Statement versions of the above
  protected def innerStms: Seq[Stm] = innerScope.flatMap(stmFromNodeId)
  protected def availStms: Seq[Stm] = availableStms.flatMap(stmFromNodeId)

  private def withInnerScope[A](scope: Seq[Int])(body: => A): A = {
    val saveInner = innerScope
    innerScope = scope
    val result = body
    innerScope = saveInner
    result
  }

  protected def withInnerStms[A](scope: Seq[Stm])(body: => A): A = {
    val ids = scope.map(_.rhs.id)
    withInnerScope(ids){ body }
  }

  private def scopeSanityCheck(block: Block[_], scope: Seq[Int]): Unit = {
    val observable = block.effectful.map(defOf).map(_.id).distinct // node ids for effect producers
    val actual = observable intersect scope
    val missing = observable diff actual
    if (missing.nonEmpty) {
      val expectedStms = observable.flatMap{s => stmFromNodeId(s)}
      val actualStms = actual.flatMap{s => stmFromNodeId(s)}
      val missingStms = missing.flatMap{s => stmFromNodeId(s)}
      throw new EffectsOrderException(block.result, expectedStms, actualStms, missingStms)
    }
  }

  final protected def visitStms(stms: Seq[Stm]): Unit = stms.foreach(visitStm)

  final protected def blockContents(block: Block[_]): Seq[Stm] = traverseStmsInBlock(block, {stms => stms})

  final protected def traverseStmsInBlock(block: Block[_]): Unit = traverseStmsInBlock(block, visitStms)
  final protected def traverseStmsInBlock[A](block: Block[_], func: Seq[Stm] => A): A = {
    val inputs = block.inputs.map(defOf).map(_.id)
    withInnerScope(availableStms diff inputs) {
      val allDependencies = syms(block.result +: block.effectful)
      // Result and scheduling dependencies
      val schedule = IR.getLocalSchedule(availableNodes = availableStms, result = allDependencies.map(_.id))
      scopeSanityCheck(block, schedule)

      // Delay all other symbols as part of the inner scope
      withInnerScope(availableStms diff schedule) {
        val stms = schedule.flatMap { nodeId => stmFromNodeId(nodeId) }
        func(stms)
      }
    }
  }

  protected def visitStm(stm: Stm): Unit = stm.rhs.blocks.foreach {blk => visitBlock(blk) }
  protected def visitBlock[S](block: Block[S]): Block[S] = { traverseStmsInBlock(block); block }

  protected def getCustomSchedule(scope: Seq[Stm], result: Seq[Exp[_]]): Seq[Stm] = {
    IR.getLocalSchedule(scope.map(_.rhs.id), syms(result).map(_.id)).flatMap(stmFromNodeId)
  }
}
