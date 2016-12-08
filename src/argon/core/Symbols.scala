package argon.core

import argon.graphs.Edge
import scala.annotation.unchecked.uncheckedVariance

trait Symbols extends Metadata with Base with Reporting { self: Statements =>

  case class SrcCtxs(pos: List[SrcCtx]) extends Metadata[SrcCtxs] { def mirror(f:Tx) = this }
  object ctxsOf {
    def apply(x: Sym[_]): List[SrcCtx] = metadata[SrcCtxs](x).map(_.pos).getOrElse(Nil)
    def update(x: Sym[_], ctx: List[SrcCtx]) = metadata.add(x, SrcCtxs(ctx))
  }
  def mpos(pos: List[SrcCtx]) = pos.head
  def mpos(s: Sym[_]) = ctxsOf(s).head

  /*case class Value[C](c: C, isConst: Boolean) extends Metadata[Value[C]] { def mirror(f:Tx) = this }
  private def setConst[C](x: Sym[_], c: C) = metadata.add(x, Value(c,isConst=true))
  private def setParam[C](x: Sym[_], c: C) = metadata.add(x, Value(c,isConst=false))*/

  abstract class Staged[T] {
    def wrap(x: Sym[T]): T
    def unwrap(x: T): Sym[T]
    def typeArguments: List[Staged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
  }

  def stg[T:Staged] = implicitly[Staged[T]]
  def mstg[A,B](x: Staged[A]): Staged[B] = x.asInstanceOf[Staged[B]]

  def wrap[T:Staged](s: Sym[T]): T = implicitly[Staged[T]].wrap(s)
  def unwrap[T:Staged](x: T): Sym[T] = implicitly[Staged[T]].unwrap(x)

  def wrap[T:Staged](xs: List[Sym[T]]): List[T] = xs.map{t => implicitly[Staged[T]].wrap(t) }
  def unwrap[T:Staged](xs: List[T]): List[Sym[T]] = xs.map{t => implicitly[Staged[T]].unwrap(t) }

  implicit class StagedTypeOps[T:Staged](x: T) {
    def s: Sym[T] = implicitly[Staged[T]].unwrap(x)
  }

  /*implicit class StagedOps[T](x: T)(implicit staged: Staged[T]) {
    def asConst(v:staged.Const): T = { setConst(staged)(x, v); x }
    def asParam(v:staged.Const): T = { setParam(staged)(x, v); x }
    def isConst = x match {case Const(_) => true; case _ => false }
    def isParam = x match {case Param(_) => true; case _ => false }
    def getValue = x match {case Param(c) => Some(c); case _ => None }
  }*/

  final class Sym[+T:Staged] extends Edge {
    def tp: Staged[T @uncheckedVariance] = stg[T]
    override def hashCode(): Int = id

    override def equals(x: Any) = x match {
      case that: Sym[_] => this.id == that.id
      case _ => false
    }
    override def toString = s"x$id"

    def withCtx(ctx: SrcCtx): Sym[T] = { ctxsOf(this) = ctx +: ctxsOf(this); this }
    def setCtx(ctx: SrcCtx): Sym[T] = { ctxsOf(this) = List(ctx); this }
  }
  private[core] def symbol[T:Staged] = new Sym[T]

  /** Compiler debugging **/
  override def readable(x: Any): String = x match {
    case s: Sym[_] => s"x${s.id}"
    case s: SrcCtxs => mpos(s.pos).toString()
    case t: Staged[_] =>
      val tArgs = if (t.typeArguments.nonEmpty)
        t.typeArguments.map(readable).mkString("[",",","]")
      else ""
      readable(t.stagedClass) + tArgs

    case _ => super.readable(x)
  }

}
