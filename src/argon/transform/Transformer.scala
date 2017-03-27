package argon.transform

import argon.core.{ArgonExceptions, Definitions}

abstract class Transformer { self =>
  val IR: Definitions with ArgonExceptions
  import IR._
  protected val f = this.asInstanceOf[Tx]
  def apply[T](e: Exp[T]): Exp[T] = transformExp(e)(mtyp(e.tp))

  def apply[T:Type](b: Block[T]): Exp[T] = inlineBlock(b)

  def apply[T](xs: List[Exp[T]]): List[Exp[T]] = xs.map{x => this.apply(x)}
  def apply[T](xs: Seq[Exp[T]]): Seq[Exp[T]] = xs.map{x => this.apply(x)}
  def apply[T](x: Option[Exp[T]]): Option[Exp[T]] = x.map{z => this.apply(z) }

  def tx(xs: List[Exp[_]]): List[Exp[_]] = xs.map{x => f(x) }
  def tx(xs: Set[Exp[_]]): Set[Exp[_]] = xs.map{x => f(x) }
  def tx(xs: Seq[Exp[_]]): Seq[Exp[_]] = xs.map{x => f(x) }

  def txSyms(xs: Set[Sym[_]]): Set[Sym[_]] = syms(xs.map{x => f(x)}).toSet

  protected def inlineBlock[T:Type](b: Block[T]): Exp[T]
  protected def transformBlock[T:Type](b: Block[T]): Block[T]
  protected def transformExp[T:Type](s: Exp[T]): Exp[T]

  /** Helper functions for mirroring **/

  // Assumes an Op is never mirrored to a Def with multiple lhs...
  final def mirror[T:Type](lhs: Sym[T], rhs: Op[T]): Exp[T] = {
    mirror(List(lhs), rhs).head.asInstanceOf[Exp[T]]
  }

  def transferMetadata(a: Exp[_], b: Exp[_]): Unit = {
    val m2 = mirror(metadata.get(a))
    metadata.add(b, m2)
    //metadata.set(b, m2) // Want to preserve effects, dependencies set during mirroring
  }

  // FIXME: Hack: only mirror metadata if the symbol is new (did not exist before starting mirroring)
  // Assumption: If the symbol we get back from cloning/mirroring had already been created by this
  // point, the mirrored symbol underwent a rewrite rule or CSE. The correct thing to do here is
  // to keep the previously created symbol's metadata, not the mirrored version of lhs's.
  final protected def transferMetadataIfNew(lhs: List[Exp[_]])(tx: => List[Exp[_]]): (List[Exp[_]], List[Boolean]) = {
    val id = IR.curEdgeId
    val lhs2 = tx
    val out = lhs.zip(lhs2).map{
      case (sym: Exp[_], sym2: Sym[_]) if sym2.id >= id =>
        transferMetadata(sym, sym2)
        (sym2, true)
      case (sym, sym2) =>
        (sym2, false)
    }
    (out.map(_._1), out.map(_._2))
  }
  final protected def transferMetadataIfNew[T](lhs: Exp[T])(tx: => Exp[T]): (Exp[T], Boolean) = {
    val id = IR.curEdgeId
    val lhs2 = tx
    (lhs, lhs2) match {
      case (sym: Exp[_], sym2: Sym[_]) if sym2.id >= id =>
        transferMetadata(sym, sym2)
        (lhs2, true)
      case _ =>
        (lhs2, false)
    }
  }


  final def mirror(lhs: List[Sym[_]], rhs: Def): List[Exp[_]] = {
    log(c"Mirror: $lhs = $rhs")
    lhs.foreach{s =>
      if (lhs.length > 1) log(c"$s")
      metadata.get(s).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    val (lhs2, _) = transferMetadataIfNew(lhs){ rhs.mirrorNode(lhs, self.asInstanceOf[Tx]) }


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
  final def mirror[M<:Metadata[_]](m: M): M = m.mirror(self.asInstanceOf[Tx]).asInstanceOf[M]

}
