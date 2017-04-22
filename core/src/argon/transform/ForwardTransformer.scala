package argon.transform

import argon.core.Staging
import argon.traversal.Traversal

trait ForwardTransformer extends SubstTransformer with Traversal { self =>
  val IR: Staging
  import IR._

  val allowPretransform = false       // Need to explicitly enable this
  final override val recurse = Never  // Mirroring already guarantees we always recursively visit scopes

  /**
    * Determine a substitution rule for the given symbol, op pair
    * By default, the rule is to mirror the node and symbol
    * @return the symbol which should replace lhs
    */
  def transform[T:Type](lhs: Sym[T], rhs: Op[T])(implicit ctx: SrcCtx): Exp[T] = {
    mirror(List(lhs), rhs).head.asInstanceOf[Exp[T]]
  }

  /**
    * Determine and register substitution rules for the given "fat" definition
    *
    * NOTE: TP cases are simple, as it is expected that it will result in a substitution rule A => A'
    * For TTP cases, any number of lhs symbols can theoretically be transformed to any other number.
    * Therefore, substitution rules must be much more explicitly managed
    */
  def transformFat(lhs: Seq[Sym[_]], rhs: Def)(implicit ctx: SrcCtx): Unit = {
    val lhs2 = mirror(lhs, rhs)
    assert(lhs.length == lhs2.length)
    lhs.zip(lhs2).foreach{case (s, s2) => register(s -> s2) }
  }

  /**
    * Visit and transform each statement in the given block WITHOUT creating a staging scope
    */
  override protected def inlineBlock[T:Type](b: Block[T]): Exp[T] = {
    inlineBlock(b, {stms => visitStms(stms); f(b.result) })
  }

  /**
    * Visit and transform each statement in the given block, creating a new Staged block
    * with the transformed statements
    */
  override protected def transformBlock[T:Type](b: Block[T]): Block[T] = {
    transformBlock(b, {stms => visitStms(stms); f(b.result) })
  }

  /**
    * Visit and perform some transformation `func` over all statements in the block, returning a result symbol
    * WITHOUT creating a staging scope.
    */
  final protected def inlineBlock[T:Type](b: Block[T], func: Seq[Stm] => Exp[T]): Exp[T] = {
    tab += 1
    val inputs2 = syms(f.tx(b.inputs)).map(stmOf)
    val result: Exp[T] = withInnerStms(availStms diff inputs2) {
      traverseStmsInBlock(b, func)
    }
    tab -= 1
    result
  }

  /**
    * Visit and perform some transformation `func` over all statements in the block, returning a new staged
    * block with the resulting transformed statements. The return Exp[T] of func will be the result symbol of the
    * new block.
    */
  final protected def transformBlock[T:Type](b: Block[T], func: Seq[Stm] => Exp[T]): Block[T] = {
    val inputs = syms(f.tx(b.inputs))
    createBlock({ inlineBlock(b,func) }, inputs, b.temp)
  }

  /**
    * Perform inlining while "mangling" the given block using the given statement transformation function.
    * No new block is created, and the return type does not have to be an Exp[T]
    * Note that this means the return types may be entirely different - use with caution.
    */
  final protected def mangleBlock[T:Type, R](b: Block[T], func: Seq[Stm] => R): R = {
    tab += 1
    val inputs2 = syms(f.tx(b.inputs)).map(stmOf)
    val result = withInnerStms(availStms diff inputs2) {
      traverseStmsInBlock(b, func)
    }
    tab -= 1
    result
  }


  /** Traversal functions **/
  final override protected def visitBlock[S](b: Block[S]): Block[S] = {
    tab += 1
    val b2 = transformBlock(b)(mtyp(b.tp))
    assert(b2.tp == b.tp)
    tab -= 1
    b2
  }

  final override protected def visit(lhs: Sym[_], rhs: Op[_]) = {
    createSubstRule(lhs, rhs.asInstanceOf[Op[Any]])(mtyp(lhs.tp), lhs.ctx)
  }

  private def createSubstRule[T:Type](lhs: Sym[T], rhs: Op[T])(implicit ctx: SrcCtx): Unit = {
    val lhs2 = if (f(lhs) == lhs) {
      val lhs2 = transform(lhs, rhs)

      // Substitution must not have any rule for lhs besides (optionally) lhs -> lhs2
      if (f(lhs) != lhs && f(lhs) != lhs2) throw new IllegalSubstException(name, lhs, f(lhs), lhs2)
      lhs2
    }
    else {
      // Pretransformed case: Someone else has already mirrored/transformed us!
      // Case 1: Multiple traversals of the same symbol in different scopes
      //   This can occur when CSE causes creation of a common node across two cold scopes.
      //   The correct thing to do in this case is use the existing substitution rule.
      //   However, the rule for case 2 should also work.
      //
      // Case 2: Some higher scope has a block which includes us, and they've already gone through and
      //   mirrored some or all of the nodes in that block before traversing the block
      //   The correct thing to do in this case is mirror the previously transformed node, then scrub the
      //   intermediate node (if it's different) from context lists so it doesn't appear in any effects lists.
      //if (!allowPretransform) throw new PretransformException(name, lhs, f(lhs))
      val lhs2: Exp[T] = f(lhs)
      val lhs3 = mirrorExp(lhs2)

      // Only remove if lhs2 has no existing dependents
      if (lhs3 != lhs2 && lhs != lhs2) lhs2 match {
        case sym2: Sym[_] => scrubSym(lhs2.asInstanceOf[Sym[_]])
        case _ =>
      }
      lhs3
    }

    if (lhs2 != lhs) register(lhs -> lhs2)
  }


  final override protected def visitFat(lhs: Seq[Sym[_]], rhs: Def) = transformFat(lhs, rhs)(lhs.head.ctx)

  override protected def preprocess[S:Type](block: Block[S]) = {
    subst = Map.empty // Reset substitutions across runs (if transformer used more than once)
    super.preprocess(block)
  }

  /**
    * DANGER ZONE
    * Use these methods only if you know what you're doing! (i.e. your name is David and you're not drunk)
    */
  final protected def mirrorExp[A](e: Exp[A]): Exp[A] = e match {
    case s: Sym[_] => stmOf(s) match {
      case stm @ TP(lhs, rhs) => mirror(List(lhs), rhs).head.asInstanceOf[Exp[A]]
      case stm @ TTP(List(lhs), rhs) => mirror(List(lhs), rhs).head.asInstanceOf[Exp[A]]
      case stm @ TTP(lhs, rhs) =>
        // TODO: How to handle this? Is this possible?
        throw new IllegalMirrorExpException(name, e)
    }
    case _ => e
  }

}
