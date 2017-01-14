package argon.core

import argon.graphs.{Edge, EdgeLike}
import argon.utils.escapeConst
import scala.annotation.unchecked.uncheckedVariance

trait Symbols extends Base with StagedTypes with Metadata { self: Staging =>

  case class SrcCtxs(pos: List[SrcCtx]) extends Metadata[SrcCtxs] { def mirror(f:Tx) = this }
  object ctxsOf {
    def apply(x: Exp[_]): List[SrcCtx] = metadata[SrcCtxs](x).map(_.pos).getOrElse(Nil)
    def update(x: Exp[_], ctx: List[SrcCtx]) = metadata.add(x, SrcCtxs(ctx))
  }
  def mpos(pos: List[SrcCtx]) = pos.head
  def mpos(s: Exp[_]) = ctxsOf(s).head

  /** Any staged symbol **/
  sealed abstract class Exp[+T] private[core](staged: Staged[T]) extends EdgeLike {
    def tp: Staged[T @uncheckedVariance] = staged

    def addCtx(ctx: SrcCtx) { ctxsOf(this) = ctx +: ctxsOf(this) }
    def setCtx(ctx: SrcCtx) { ctxsOf(this) = List(ctx) }
  }

  /** A staged symbol which represents a non-constant value **/
  // TODO: This could use a name change
  sealed abstract class Symbol[+T] private[core](staged: Staged[T]) extends Exp[T](staged) with Edge {
    override def hashCode(): Int = id
    override def equals(x: Any) = x match {
      case that: Symbol[_] => this.id == that.id
      case _ => false
    }
  }

  /** Staged symbols created as bound variables **/
  class Bound[+T] private[core](staged: Staged[T]) extends Symbol[T](staged) {
    override def toString = s"b$id"
  }

  /** Staged symbols with definitions **/
  class Sym[+T] private[core](staged: Staged[T]) extends Symbol[T](staged) {
    override def toString = s"x$id"
  }



  // In compiler API, we want to be specify staged constants:
  //   def foo(x: Const[Int]) ...
  // In compiler, we want to be able to write things like:
  //   case x: Param[_] => x.c = 3
  // and be guaranteed that this is legal
  // :: Param is a special, mutable case of Const
  // TODO: Is there ever a case where a Param can't be used as a Const?

  /** A staged constant **/
  class Const[+T] private[core](x: Any)(staged: Staged[T]) extends Exp[T](staged) {
    private val _c: Any = x
    def c: Any = _c

    override def hashCode() = (tp, c).hashCode()
    override def equals(x: Any) = x match {
      case that: Const[_] => this.tp == that.tp && this.c == that.c
      case _ => false
    }

    override def toString = escapeConst(c)
  }

  /** A staged, mutable constant **/
  private var nParams = 0

  class Param[+T] private[core](x: Any)(staged: Staged[T]) extends Const[T](x)(staged) {
    private[argon] val pid = {nParams -= 1; nParams}

    private var _p: Any = x
    override def c: Any = _p
    def c_=(rhs: Any) { _p = c }
    override def toString = escapeConst(c)

    override def hashCode() = pid
    override def equals(x: Any) = x match {
      case that: Param[_] => this.pid == that.pid
      case _ => false
    }
  }

  private[core] def __sym(tp: Staged[_]): Sym[_] = new Sym(tp)
  private[core] def __bound[T:Staged]: Bound[T] = new Bound(typ[T])
  private[core] def __const[T:Staged](x: Any): Const[T] = new Const(x)(typ[T])
  private[core] def __param[T:Staged](x: Any): Param[T] = new Param(x)(typ[T])


  def constUnapply(s: Exp[_]): Option[Any] = s match {
    case const:Const[_] => Some(const.c)
    case _ => None
  }
  def paramUnapply(s: Exp[_]): Option[Any] = s match {
    case param:Param[_] => Some(param.c)
    case _ => None
  }

  object Const {
    def unapply(s: Exp[_]): Option[Any] = constUnapply(s)
  }

  object Param {
    def unapply(s: Exp[_]): Option[Any] = paramUnapply(s)
  }

  /** Compiler debugging **/
  override def readable(x: Any): String = x match {
    case s: Sym[_] => s"x${s.id}"
    case b: Bound[_] => s"b${b.id}"
    case p: Param[_] => s"Param($p)"
    case c: Const[_] => s"Const($c)"
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
