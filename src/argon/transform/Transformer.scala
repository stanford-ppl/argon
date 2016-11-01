package argon.transform

import argon.core.Definitions

abstract class Transformer {
  val IR: Definitions
  import IR._
  protected def f = this.asInstanceOf[Tx]
  def apply[S:Typ](s: S): S
  def apply[S<:Sym](s: S): S
  def apply[S<:Sym](x: List[S]): List[S] = x.map{z => apply(z)}
  def apply[S<:Sym](x: Seq[S]): Seq[S] = x.map{z => apply(z)}
  def apply[S<:Sym](x: Set[S]): Set[S] = x.map{z => apply(z)}
  def apply[S<:Sym](x: Option[S]): Option[S] = x.map{z => apply(z)}
  def apply[S:Typ](b: Block[S]): S

  def tx[T](t: T): T = (t match {
    case s: Sym       => transformSym(s)
    case b: Block[_]  => transformBlock(b)(mtyp(b.tp))
    case xs: List[_]  => xs.map{z => tx(z) }
    case xs: Set[_]   => xs.map{z => tx(z) }
    case xs: Seq[_]   => xs.map{z => tx(z) }
    case xs: Map[_,_] => xs.map{case (k,v) => tx(k) -> tx(v) }
    case _            => t
  }).asInstanceOf[T]

  def transformSym[S<:Sym](s: S): S

  def transformBlock[S:Typ](b: Block[S]): Block[S]
}
