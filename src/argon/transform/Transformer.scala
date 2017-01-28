package argon.transform

import argon.core.{ArgonExceptions, Definitions}

abstract class Transformer { self =>
  val IR: Definitions with ArgonExceptions
  import IR._
  protected def f = this.asInstanceOf[Tx]
  def apply[T](e: Exp[T]): Exp[T] = e match {
    case s: Sym[_] => transformSym(s)(mtyp(s.tp)).asInstanceOf[Exp[T]]
    case b: Bound[_] => b
    case c: Const[_] => c
  }
  def apply[T:Staged](b: Block[T]): Exp[T] = inlineBlock(b)

  def apply[T:Staged](xs: List[Exp[T]]): List[Exp[T]] = xs.map{x => this.apply(x)}
  def apply[T:Staged](xs: Seq[Exp[T]]): Seq[Exp[T]] = xs.map{x => this.apply(x)}
  def apply[T:Staged](x: Option[Exp[T]]): Option[Exp[T]] = x.map{z => this.apply(z) }

  def tx(xs: List[Exp[_]]): List[Exp[_]] = xs.map{x => f(x) }
  def tx(xs: Set[Exp[_]]): Set[Exp[_]] = xs.map{x => f(x) }
  def tx(xs: Seq[Exp[_]]): Seq[Exp[_]] = xs.map{x => f(x) }

  def txSyms(xs: Set[Sym[_]]): Set[Sym[_]] = onlySyms(xs.map{x => f(x)}).toSet

  protected def inlineBlock[T:Staged](b: Block[T]): Exp[T]
  protected def transformBlock[T:Staged](b: Block[T]): Block[T]
  protected def transformSym[T:Staged](s: Sym[T]): Exp[T]

  /** Helper functions for mirroring **/

  // Assumes an Op is never mirrored to a Def with multiple lhs...
  final protected def mirror[T:Staged](lhs: Sym[T], rhs: Op[T]): Exp[T] = {
    mirror(List(lhs), rhs).head.asInstanceOf[Exp[T]]
  }

  def transferMetadata(a: Exp[_], b: Exp[_]): Unit = {
    val m2 = mirror(metadata.get(a))
    metadata.set(b, m2)
  }

  final protected def mirror(lhs: List[Sym[_]], rhs: Def): List[Exp[_]] = {
    val id = IR.curEdgeId
    log(c"Mirror: $lhs = $rhs")
    lhs.foreach{s =>
      if (lhs.length > 1) log(c"$s")
      metadata.get(s).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    val lhs2 = rhs.mirrorNode(lhs, self.asInstanceOf[Tx])

    // FIXME: Hack: only mirror metadata if the symbol is new (did not exist before starting mirroring)
    log(c"Result: ${str(lhs2)}")
    lhs.zip(lhs2).foreach{
      case (sym: Sym[_], sym2: Sym[_]) if sym2.id >= id => transferMetadata(sym, sym2)
      case _ =>
    }
    lhs2.foreach{s2 =>
      if (lhs2.length > 1) log(c"$s2")
      metadata.get(s2).foreach{m => log(c" - ${m._1}: ${m._2}") }
    }

    lhs2
  }

  final protected def mirror(props: Map[Class[_],Metadata[_]]): Map[Class[_],Metadata[_]] = props.mapValues{m => mirror(m) }
  final protected def mirror[M<:Metadata[_]](m: M): M = m.mirror(self.asInstanceOf[Tx]).asInstanceOf[M]

}
