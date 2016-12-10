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
  def wrap[T:Staged](xs: Seq[Sym[T]]): Seq[T] = xs.map{t => implicitly[Staged[T]].wrap(t) }
  def unwrap[T:Staged](xs: Seq[T]): Seq[Sym[T]] = xs.map{t => implicitly[Staged[T]].unwrap(t) }

  implicit class StagedTypeOps[T:Staged](x: T) {
    def s: Sym[T] = implicitly[Staged[T]].unwrap(x)
  }

  /** Any staged symbol **/
  class Sym[+T]()(implicit staged: Staged[T]) extends Edge {
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
  private[core] def __sym[T](tp: Staged[T]): Sym[T] = new Sym[T]()(tp)

  // In API, we want to be able to ask for staged constants:
  //   def foo(x: Const[Int]) ...
  // In compiler, we want to be able to write things like:
  //   case x: Param[_] => x.c = 3
  // and be guaranteed that this is legal
  // :: Param is a special, mutable case of Const
  // TODO: Is there ever a case where a Param can't be used as a Const?

  /** A staged constant **/
  class Const[+T:Staged] private[core](x: Any) extends Sym[T] {
    private val _c: Any = x
    def c: Any = _c

    override def hashCode() = (tp, c).hashCode()
    override def equals(x: Any) = x match {
      case that: Const[_] => this.tp == that.tp && this.c == that.c
      case _ => false
    }
  }
  private[core] def __const[T](x: Any)(staged: Staged[T]): Const[T] = new Const[T](x)(staged)

  /** A staged, mutable constant **/
  // TODO: Sets, HashMaps of Params? (hashCode can change...)
  class Param[+T:Staged] private[core](x: Any) extends Const[T](x) {
    private var _p: Any = x
    override def c: Any = _p
    def c_=(rhs: Any) { _p = c }
  }
  private[core] def __param[T](x: Any)(staged: Staged[T]): Param[T] = new Param[T](x)(staged)


  def constUnapply(s: Sym[_]): Option[Any] = s match {
    case const:Const[_] => Some(const.c)
    case _ => None
  }
  def paramUnapply(s: Sym[_]): Option[Any] = s match {
    case param:Param[_] => Some(param.c)
    case _ => None
  }

  object Const {
    def unapply(s: Sym[_]): Option[Any] = constUnapply(s)
  }

  object Param {
    def unapply(s: Sym[_]): Option[Any] = paramUnapply(s)
  }

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
