package argon.transform

import argon.core._

trait Transformer { self =>
  var IR: State
  implicit def __state: State = IR
  def name: String

  protected val f = this //.asInstanceOf[Tx]
  final def apply[T](e: Exp[T]): Exp[T] = transformExp(e)(mtyp(e.tp))

  // Transform the given block, converting it to a stageable function
  final def apply[R:Type](b: Block[R]): () => Exp[R] = blockToFunction0(b, copy=false)
  final def apply[A,R:Type](b: Lambda1[A,R]): Exp[A] => Exp[R] = lambda1ToFunction1(b, copy=false)
  final def apply[A,B,R:Type](b: Lambda2[A,B,R]): (Exp[A],Exp[B]) => Exp[R] = lambda2ToFunction2(b, copy=false)
  final def apply[A,B,C,R:Type](b: Lambda3[A,B,C,R]): (Exp[A],Exp[B],Exp[C]) => Exp[R] = lambda3ToFunction3(b, copy=false)
  final def apply[A,B,C,D,R:Type](b: Lambda4[A,B,C,D,R]): (Exp[A],Exp[B],Exp[C],Exp[D]) => Exp[R] = lambda4ToFunction4(b, copy=false)
  final def apply[A,B,C,D,E,R:Type](b: Lambda5[A,B,C,D,E,R]): (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]) => Exp[R] = lambda5ToFunction5(b, copy=false)
  final def apply[A,B,C,D,E,F,R:Type](b: Lambda6[A,B,C,D,E,F,R]): (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E],Exp[F]) => Exp[R] = lambda6ToFunction6(b, copy=false)

  final def apply[T](xs: List[Exp[T]]): List[Exp[T]] = xs.map{x => this.apply(x)}
  final def apply[T](xs: Seq[Exp[T]]): Seq[Exp[T]] = xs.map{x => this.apply(x)}
  final def apply[T](x: Option[Exp[T]]): Option[Exp[T]] = x.map{z => this.apply(z) }

  // Inline the given block (should be called within a staging scope of some kind)
  final def tx[T:Type](x: Block[T]): Exp[T] = { val func = f(x); func() }

  final def tx(xs: List[Exp[_]]): List[Exp[_]] = xs.map{x => f(x) }
  final def tx(xs: Set[Exp[_]]): Set[Exp[_]] = xs.map{x => f(x) }
  final def tx(xs: Seq[Exp[_]]): Seq[Exp[_]] = xs.map{x => f(x) }

  final def txSyms(xs: Set[Sym[_]]): Set[Sym[_]] = syms(xs.map{x => f(x)}).toSet

  // TODO: Something appears to be broken with .restage, not sure what yet though

  implicit class BlockOps[R](block: Block[R]) {
    def inline: Exp[R] = { val func = blockToFunction0(block, copy=true); func() }
    //def restage(): Block[R] = transformBlock(block)
    def toFunction0: () => Exp[R] = blockToFunction0(block, copy=true)
  }
  implicit class Lambda1Ops[A,R](lambda1: Lambda1[A,R]) {
    def inline(a: Exp[A]): Exp[R] = { val func = lambda1ToFunction1(lambda1, copy=true); func(a) }
    /*def restage(a: Exp[A]): Lambda1[A,R] = {
      stageLambda1(a)({ lambda1.inline(a) }, lambda1.temp, lambda1.isolated, lambda1.seal)
    }*/
    def toFunction1: Exp[A] => Exp[R] = lambda1ToFunction1(lambda1, copy=true)
  }
  implicit class Lambda2Ops[A,B,R](lambda2: Lambda2[A,B,R]) {
    def inline(a: Exp[A], b: Exp[B]): Exp[R] = { val func = lambda2ToFunction2(lambda2, copy=true); func(a,b) }
    /*def restage(a: Exp[A], b: Exp[B]): Lambda2[A,B,R] = {
      stageLambda2(a,b)({ lambda2.inline(a,b) }, lambda2.temp, lambda2.isolated, lambda2.seal)
    }*/
    def toFunction2: (Exp[A], Exp[B]) => Exp[R] = lambda2ToFunction2(lambda2, copy=true)
  }
  implicit class Lambda3Ops[A,B,C,R](lambda3: Lambda3[A,B,C,R]) {
    def inline(a: Exp[A], b: Exp[B], c: Exp[C]): Exp[R] = { val func = lambda3ToFunction3(lambda3, copy=true); func(a,b,c) }
    /*def restage(a: Exp[A], b: Exp[B], c: Exp[C]): Lambda3[A,B,C,R] = {
      stageLambda3(a,b,c)({ lambda3.inline(a,b,c) }, lambda3.temp, lambda3.isolated, lambda3.seal)
    }*/
    def toFunction3: (Exp[A], Exp[B], Exp[C]) => Exp[R] = lambda3ToFunction3(lambda3, copy=true)
  }
  implicit class Lambda4Ops[A,B,C,D,R](lambda4: Lambda4[A,B,C,D,R]) {
    def inline(a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]): Exp[R] = { val func = lambda4ToFunction4(lambda4, copy = true); func(a,b,c,d) }
    /*def restage(a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]): Lambda4[A,B,C,D,R] = {
      stageLambda4(a,b,c,d)({ lambda4.inline(a,b,c,d) }, lambda4.temp, lambda4.isolated, lambda4.seal)
    }*/
    def toFunction4: (Exp[A], Exp[B], Exp[C], Exp[D]) => Exp[R] = lambda4ToFunction4(lambda4, copy = true)
  }
  implicit class Lambda5Ops[A,B,C,D,E,R](lambda5: Lambda5[A,B,C,D,E,R]) {
    def inline(a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E]): Exp[R] = { val func = lambda5ToFunction5(lambda5, copy = true); func(a,b,c,d,e) }
    def toFunction5: (Exp[A], Exp[B], Exp[C], Exp[D], Exp[E]) => Exp[R] = lambda5ToFunction5(lambda5, copy = true)
  }
  implicit class Lambda6Ops[A,B,C,D,E,F,R](lambda6: Lambda6[A,B,C,D,E,F,R]) {
    def inline(a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D], e: Exp[E], f: Exp[F]): Exp[R] = { val func = lambda6ToFunction6(lambda6, copy = true); func(a,b,c,d,e,f) }
    def toFunction5: (Exp[A], Exp[B], Exp[C], Exp[D], Exp[E], Exp[F]) => Exp[R] = lambda6ToFunction6(lambda6, copy = true)
  }

  protected def blockToFunction0[R](b: Block[R], copy: Boolean): () => Exp[R] = () => inlineBlock(b)
  protected def lambda1ToFunction1[A,R](b: Lambda1[A,R], copy: Boolean): Exp[A] => Exp[R]
  protected def lambda2ToFunction2[A,B,R](b: Lambda2[A,B,R], copy: Boolean): (Exp[A],Exp[B]) => Exp[R]
  protected def lambda3ToFunction3[A,B,C,R](b: Lambda3[A,B,C,R], copy: Boolean): (Exp[A],Exp[B],Exp[C]) => Exp[R]
  protected def lambda4ToFunction4[A,B,C,D,R](b: Lambda4[A,B,C,D,R], copy: Boolean): (Exp[A],Exp[B],Exp[C],Exp[D]) => Exp[R]
  protected def lambda5ToFunction5[A,B,C,D,E,R](b: Lambda5[A,B,C,D,E,R], copy: Boolean): (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]) => Exp[R]
  protected def lambda6ToFunction6[A,B,C,D,E,F,R](b: Lambda6[A,B,C,D,E,F,R], copy: Boolean): (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E],Exp[F]) => Exp[R]

  /**
    * Visit and transform each statement in the given block WITHOUT creating a staging scope
    *
    * Override this function for custom block transformation rules
    */
  protected def inlineBlock[T](b: Block[T]): Exp[T]

  /**
    * Visit and perform some transformation `func` over all statements in the block, returning a result symbol
    * WITHOUT creating a staging scope.
    */
  protected def inlineBlockWith[T](b: Block[T], func: Seq[Stm] => Exp[T]): Exp[T]

  /**
    * Visit and transform each statement in the given block, creating a new Staged block
    * with the transformed statements
    *
    * Utility function - calls inlineBlock in all cases
    */
  final protected def transformBlock[T, B[T]<:Block[T]](block: B[T]): B[T] = (block match {
    case Lambda1(input,_,_,_,props)       => stageLambda1(f(input))({ inlineBlock(block) }, props)
    case Lambda2(a,b, _,_,_,props)        => stageLambda2(f(a),f(b))({ inlineBlock(block) }, props)
    case Lambda3(a,b,c,_,_,_,props)       => stageLambda3(f(a),f(b),f(c))( {inlineBlock(block) }, props)
    case Lambda4(a,b,c,d,_,_,_,props)     => stageLambda4(f(a),f(b),f(c),f(d))({ inlineBlock(block)}, props)
    case Lambda5(a,b,c,d,e,_,_,_,props)   => stageLambda5(f(a),f(b),f(c),f(d),f(e))({ inlineBlock(block)}, props)
    case Lambda6(a,b,c,d,e,x,_,_,_,props) => stageLambda6(f(a),f(b),f(c),f(d),f(e),f(x))({ inlineBlock(block)}, props)
    case Block(inputs,_,_,_,props)        => stageLambdaN(f.tx(inputs), { inlineBlock(block) }, props)
  }).asInstanceOf[B[T]]


  protected def transformExp[T:Type](s: Exp[T]): Exp[T]


  /** Helper functions for mirroring **/
  def transferMetadata(a: Exp[_], b: Exp[_]): Unit = {
    val m2 = mirror(metadata.get(a))
    b.name = a.name
    b.prevNames = (state.paddedPass(state.pass-1),a.toString) +: a.prevNames
    metadata.add(b, m2) // Want to preserve effects, dependencies set during mirroring
  }

  // FIXME: Hack: only mirror metadata if the symbol is new (did not exist before starting mirroring)
  // Assumption: If the symbol we get back from cloning/mirroring had already been created by this
  // transformer, the mirrored symbol underwent a rewrite rule or CSE. The correct thing to do here is
  // to keep the previously created symbol's metadata, not the mirrored version of lhs's.
  final protected def transferMetadataIfNew(lhs: Seq[Exp[_]])(tx: => Seq[Exp[_]]): (Seq[Exp[_]], Seq[Boolean]) = {
    val id = IR.graph.curEdgeId
    val lhs2 = tx
    val out = lhs.zip(lhs2).map{
      case (sym: Exp[_], sym2: Sym[_]) if sym2.id >= id || sym == sym2 =>
        transferMetadata(sym, sym2)
        (sym2, true)
      case (sym, sym2) =>
        (sym2, false)
    }
    (out.map(_._1), out.map(_._2))
  }
  final protected def transferMetadataIfNew[T](lhs: Exp[T])(tx: => Exp[T]): (Exp[T], Boolean) = {
    val id = IR.graph.curEdgeId
    val lhs2 = tx
    (lhs, lhs2) match {
      case (sym: Exp[_], sym2: Sym[_]) if sym2.id >= id || sym == sym2 =>
        transferMetadata(sym, sym2)
        (lhs2, true)
      case _ =>
        (lhs2, false)
    }
  }

  final def mirror[T](lhs: Sym[T], rhs: Op[T]): Exp[T] = mirror(Seq(lhs),rhs).head.asInstanceOf[Exp[T]]

  def mirror(lhs: Seq[Sym[_]], rhs: Def): Seq[Exp[_]] = {
    log(c"Mirror: $lhs = $rhs")
    lhs.foreach{s =>
      if (lhs.length > 1) log(c"$s")
      metadata.get(s).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    val (lhs2, _) = transferMetadataIfNew(lhs){ rhs.mirrorNode(self) }

    log(c"Result: ${str(lhs2)}")
    lhs2.foreach{s2 =>
      if (lhs2.length > 1) log(c"$s2")
      metadata.get(s2).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    lhs2
  }

  final def mirror(props: Map[Class[_],Metadata[_]]): Map[Class[_],Metadata[_]] = {
    // Somehow, using mapValues here causes the metadata to be call by name, causing it to re-mirror on every fetch...
    // Also, scala docs lie about the return type of collect on flatMap apparently
    props.collect{case (key,meta) if !meta.ignoreOnTransform => key -> mirror(meta) }.toMap
  }
  final def mirror[M<:Metadata[_]](m: M): M = m.mirror(self).asInstanceOf[M]

}
