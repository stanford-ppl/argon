package argon.transform

import argon.core.Definitions

abstract class Transformer {
  val IR: Definitions
  import IR._
  protected def f = this.asInstanceOf[Tx]
  def apply[T:Staged](s: Sym[T]): T
  def apply[T:Staged](b: Block[T]): T
  def apply[T:Staged](xs: List[Sym[T]]): List[T] = xs.map{x => this.apply(x)}
  def apply[T:Staged](x: Option[Sym[T]]): Option[T] = x.map{z => this.apply(z) }
  /*def apply(x: List[Sym[_]]): List[Sym[_]] = x.map{z => tx(z)}
  def apply(x: Seq[Sym[_]]): Seq[Sym[_]] = x.map{z => tx(z)}
  def apply(x: Set[Sym[_]]): Set[Sym[_]] = x.map{z => tx(z)}
  def apply[T:Staged](x: Option[Sym[T]]): Option[Sym[T]] = x.map{z => tx(z)}
  */

  def tx[T](t: T): T = (t match {
    case s: Sym[_]    => transformSym(s)(s.tp)
    case b: Block[_]  => transformBlock(b)(mstg(b.tp))
    case xs: List[_]  => xs.map{z => tx(z) }
    case xs: Set[_]   => xs.map{z => tx(z) }
    case xs: Seq[_]   => xs.map{z => tx(z) }
    case xs: Map[_,_] => xs.map{case (k,v) => tx(k) -> tx(v) }
    case _            => t
  }).asInstanceOf[T]

  def transformSym[T:Staged](s: Sym[T]): Sym[T]

  def transformBlock[T:Staged](b: Block[T]): Block[T]
}
