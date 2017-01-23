package argon.transform

import argon.core.Staging
import argon.traversal.Traversal
import org.virtualized.SourceContext

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
  def transform[T:Staged](lhs: Sym[T], rhs: Op[T])(implicit ctx: SrcCtx): Exp[T] = {
    mirror(List(lhs), rhs).head.asInstanceOf[Exp[T]]
  }

  /**
    * Determine and register substitution rules for the given "fat" definition
    *
    * NOTE: TP cases are simple, as it is expected that it will result in a substitution rule A => A'
    * For TTP cases, any number of lhs symbols can theoretically be transformed to any other number.
    * Therefore, substitution rules must be much more explicitly managed
    */
  def transformFat(lhs: List[Sym[_]], rhs: Def)(implicit ctx: SrcCtx): Unit = {
    val lhs2 = mirror(lhs, rhs)
    assert(lhs.length == lhs2.length)
    lhs.zip(lhs2).foreach{case (s, s2) => register(s -> s2) }
  }

  final override protected def inlineBlock[T:Staged](b: Block[T]): Exp[T] = inlineBlock(b, visitStms)
  final override protected def transformBlock[T:Staged](b: Block[T]): Block[T] = transformBlock(b, visitStms)

  final protected def inlineBlock[T:Staged](b: Block[T], func: Seq[Stm] => Unit): Exp[T] = {
    tab += 1
    val inputs2 = onlySyms(f.tx(b.inputs))
    val result: Exp[T] = withInnerScope(availableStms diff inputs2) {
      traverseStmsInBlock(b, func)
      f(b.result)
    }
    tab -= 1
    result
  }

  final protected def transformBlock[T:Staged](b: Block[T], func: Seq[Stm] => Unit): Block[T] = {
    val inputs = onlySyms(f.tx(b.inputs))
    stageLambda(inputs:_*){ inlineBlock(b, func) }
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
    createSubstRule(lhs, rhs.asInstanceOf[Op[Any]])(mtyp(lhs.tp), ctxOrHere(lhs))
  }

  private def createSubstRule[T:Staged](lhs: Sym[T], rhs: Op[T])(implicit ctx: SrcCtx): Unit = {
    val lhs2 = if (f(lhs) == lhs) {
      val lhs2 = transform(lhs, rhs)

      // Substitution must not have any rule for lhs besides (optionally) lhs -> lhs2
      if (f(lhs) != lhs && f(lhs) != lhs2) throw new IllegalSubstException(name, lhs, f(lhs), lhs2)
      lhs2
    }
    else {
      // Pretransformed case: Someone else has already mirrored/transformed us!
      // Assumed case: Some higher scope has a block which includes us, and they've already gone through and
      // mirrored some or all of the nodes in that block before traversing the block
      // The correct thing to do here is mirror the previously transformed node, then scrub the intermediate node from
      // the IR def and context lists so it doesn't appear in any effects lists.
      if (!allowPretransform) throw new PretransformException(name, lhs, f(lhs))

      val lhs2: Exp[T] = f(lhs)
      val lhs3 = mirrorExp(lhs2)
      if (lhs3 != lhs2 && lhs != lhs2 && lhs2.isInstanceOf[Sym[_]]) scrubSym(lhs2.asInstanceOf[Sym[_]])
      lhs3
    }

    if (lhs2 != lhs) register(lhs -> lhs2)
  }


  final override protected def visitFat(lhs: List[Sym[_]], rhs: Def) = transformFat(lhs, rhs)(ctxOrHere(lhs.head))

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

  final protected def scrubSym(s: Sym[_]): Unit = {
    IR.scrubSymbol(s)
    innerScope = innerScope.filterNot(_ == s.id)
  }
}
