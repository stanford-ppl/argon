package argon.core

import argon.graphs.Edge
import scala.annotation.unchecked.uncheckedVariance

trait Symbols extends Base with StagedTypes with Metadata { self: Staging =>

  case class SrcCtxs(pos: List[SrcCtx]) extends Metadata[SrcCtxs] { def mirror(f:Tx) = this }
  object ctxsOf {
    def apply(x: Sym[_]): List[SrcCtx] = metadata[SrcCtxs](x).map(_.pos).getOrElse(Nil)
    def update(x: Sym[_], ctx: List[SrcCtx]) = metadata.add(x, SrcCtxs(ctx))
  }
  def mpos(pos: List[SrcCtx]) = pos.head
  def mpos(s: Sym[_]) = ctxsOf(s).head

  /** Any staged symbol **/
  class Sym[+T] private[core](staged: Staged[T]) extends Edge {
    def tp: Staged[T @uncheckedVariance] = staged
    override def hashCode(): Int = id

    override def equals(x: Any) = x match {
      case that: Sym[_] => this.id == that.id
      case _ => false
    }
    override def toString = s"x$id"

    def withCtx(ctx: SrcCtx): Sym[T] = { ctxsOf(this) = ctx +: ctxsOf(this); this }
    def setCtx(ctx: SrcCtx): Sym[T] = { ctxsOf(this) = List(ctx); this }
  }

  // In compiler API, we want to be specify staged constants:
  //   def foo(x: Const[Int]) ...
  // In compiler, we want to be able to write things like:
  //   case x: Param[_] => x.c = 3
  // and be guaranteed that this is legal
  // :: Param is a special, mutable case of Const
  // TODO: Is there ever a case where a Param can't be used as a Const?

  /** A staged constant **/
  class Const[+T] private[core](x: Any)(staged: Staged[T]) extends Sym[T](staged) {
    private val _c: Any = x
    def c: Any = _c

    override def hashCode() = (tp, c).hashCode()
    override def equals(x: Any) = x match {
      case that: Const[_] => this.tp == that.tp && this.c == that.c
      case _ => false
    }
  }

  /** A staged, mutable constant **/
  // TODO: Sets, HashMaps of Params? (hashCode can change...)
  class Param[+T] private[core](x: Any)(staged: Staged[T]) extends Const[T](x)(staged) {
    private var _p: Any = x
    override def c: Any = _p
    def c_=(rhs: Any) { _p = c }
  }

  private[core] def __sym(tp: Staged[_]): Sym[_] = new Sym(tp)
  private[core] def __const(x: Any)(staged: Staged[_]): Const[_] = new Const(x)(staged)
  private[core] def __param(x: Any)(staged: Staged[_]): Param[_] = new Param(x)(staged)


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

  override def userReadable(x: Any): String = x match {
    case t: Staged[_] =>
      val tArgs = if (t.typeArguments.nonEmpty)
        t.typeArguments.map(userReadable).mkString("[",",","]")
      else ""
      userReadable(t.stagedClass) + tArgs
    case _ => super.userReadable(x)
  }
}
