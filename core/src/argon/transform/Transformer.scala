package argon.transform

import argon._

abstract class Transformer { self =>
  val IR: State //Definitions with ArgonExceptions
  import IR._

  protected val f = this //.asInstanceOf[Tx]
  def apply[T](e: Exp[T]): Exp[T] = transformExp(e)(mtyp(e.tp))

  def apply[R:Type](b: Block[R]): () => Exp[R] = inlineBlock(b)
  def apply[A,R:Type](b: Lambda1[A,R]): Exp[A] => Exp[R] = inlineLambda(b)
  def apply[A,B,R:Type](b: Lambda2[A,B,R]): (Exp[A],Exp[B]) => Exp[R] = inlineLambda(b)
  def apply[A,B,C,R:Type](b: Lambda3[A,B,C,R]): (Exp[A],Exp[B],Exp[C]) => Exp[R] = inlineLambda(b)
  def apply[A,B,C,D,R:Type](b: Lambda4[A,B,C,D,R]): (Exp[A],Exp[B],Exp[C],Exp[D]) => Exp[R] = inlineLambda(b)

  def apply[T](xs: List[Exp[T]]): List[Exp[T]] = xs.map{x => this.apply(x)}
  def apply[T](xs: Seq[Exp[T]]): Seq[Exp[T]] = xs.map{x => this.apply(x)}
  def apply[T](x: Option[Exp[T]]): Option[Exp[T]] = x.map{z => this.apply(z) }

  def tx(xs: List[Exp[_]]): List[Exp[_]] = xs.map{x => f(x) }
  def tx(xs: Set[Exp[_]]): Set[Exp[_]] = xs.map{x => f(x) }
  def tx(xs: Seq[Exp[_]]): Seq[Exp[_]] = xs.map{x => f(x) }

  def txSyms(xs: Set[Sym[_]]): Set[Sym[_]] = syms(xs.map{x => f(x)}).toSet

  protected def inlineBlock[R:Type](b: Block[R]): () => Exp[R]
  protected def inlineLambda[A,R:Type](b: Lambda1[A,R]): Exp[A] => Exp[R]
  protected def inlineLambda[A,B,R:Type](b: Lambda2[A,B,R]): (Exp[A],Exp[B]) => Exp[R]
  protected def inlineLambda[A,B,C,R:Type](b: Lambda3[A,B,C,R]): (Exp[A],Exp[B],Exp[C]) => Exp[R]
  protected def inlineLambda[A,B,C,D,R:Type](b: Lambda4[A,B,C,D,R]): (Exp[A],Exp[B],Exp[C],Exp[D]) => Exp[R]


  protected def transformBlock[T:Type](b: Block[T]): Block[T]
  protected def transformExp[T:Type](s: Exp[T]): Exp[T]

  /** Helper functions for mirroring **/

  // Assumes an Op is never mirrored to a Def with multiple lhs...
  final def mirror[T:Type](lhs: Sym[T], rhs: Op[T]): Exp[T] = {
    mirror(List(lhs), rhs).head.asInstanceOf[Exp[T]]
  }

  def transferMetadata(a: Exp[_], b: Exp[_]): Unit = {
    val m2 = mirror(metadata.get(a))
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


  def mirror(lhs: Seq[Sym[_]], rhs: Def): Seq[Exp[_]] = {
    log(c"Mirror: $lhs = $rhs")
    lhs.foreach{s =>
      if (lhs.length > 1) log(c"$s")
      metadata.get(s).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    val (lhs2, _) = transferMetadataIfNew(lhs){ rhs.mirrorNode(lhs, self) }

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
