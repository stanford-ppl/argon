package argon.traversal

import argon.Config
import argon.core.Staging

trait BlockTraversal {
  val IR: Staging
  import IR._

  /** All statements defined in lower (further nested) blocks within the current traversal scope **/
  private var innerScope: Seq[NodeId] = _

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

  final protected def visitStms(stms: Seq[Stm]): Unit = stms.foreach(visitStm)

  /**
    * Gives a list of all statements defined in the scope of this block
    * Only returns statements within a single level (i.e. doesn't give contents of blocks within this block)
    * Statements are returned in the same order that they would be traversed in.
    */
  final protected def blockContents(block: Block[_]): Seq[Stm] = traverseStmsInBlock(block, {stms => stms})

  /**
    * Gives a list of symbols which are used in this block and defined outside this block
    * Also gives a list of all statements defined in this block, including all nested scopes
    * NOTE: This is likely somewhat expensive, should be used sparingly
    */
  final protected def blockInputsAndNestedContents(block: Block[_]): (Seq[Exp[_]], Seq[Stm]) = {

    // NOTE: Can't use repeated blockContents calls here, as getting schedule relies on innerScope being updated
    def definedInBlock(x: Block[_]): Seq[Stm] = {
      traverseStmsInBlock(x, {stms => stms ++ stms.flatMap{_.rhs.blocks.flatMap(definedInBlock)} })
    }

    val stms = definedInBlock(block)
    val used = stms.flatMap(_.rhs.inputs) ++ block.inputs
    val made = stms.flatMap{stm => stm.lhs ++ stm.rhs.binds }.toSet

    val inputs = used filterNot (made contains _)

    if (Config.verbosity > 1) {
      log(c"Used:")
      used.foreach{s => log(c"  ${str(s)}")}
      log(c"Made:")
      made.foreach{s => log(c"  ${str(s)}")}
      log(c"Inputs:")
      inputs.foreach{s => log(c"  ${str(s)}")}
    }

    (inputs, stms)
  }

  final protected def traverseStmsInBlock(block: Block[_]): Unit = traverseStmsInBlock(block, visitStms)
  final protected def traverseStmsInBlock[A](block: Block[_], func: Seq[Stm] => A): A = {
    val inputs = block.inputs.map(defOf).map(_.id)
    withInnerScope(availableStms diff inputs) {
      val schedule = IR.scheduleBlock(availableStms, block)
      // Delay all other symbols as part of the inner scope
      withInnerScope(availableStms diff schedule.map(_.rhs.id)) {
        func(schedule)
      }
    }
  }

  protected def visitStm(stm: Stm): Unit = stm.rhs.blocks.foreach {blk => visitBlock(blk) }
  protected def visitBlock[S](block: Block[S]): Block[S] = { traverseStmsInBlock(block); block }

  protected def getCustomSchedule(scope: Seq[Stm], result: Seq[Exp[_]]): Seq[Stm] = {
    IR.getLocalSchedule(scope.map(_.rhs.id), syms(result).map(_.id)).flatMap(stmFromNodeId)
  }
}
