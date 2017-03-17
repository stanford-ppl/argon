package argon.core

import argon.graphs.{Edge, EdgeLike}
import argon.utils.escapeConst

import scala.annotation.unchecked.uncheckedVariance


trait Symbols extends StagedTypes with Metadata { self: Staging =>

  case class SrcCtxs(pos: List[SrcCtx]) extends Metadata[SrcCtxs] { def mirror(f:Tx) = this }
  object ctxsOf {
    def apply(x: Exp[_]): List[SrcCtx] = metadata[SrcCtxs](x).map(_.pos).getOrElse(Nil)
    def update(x: Exp[_], ctx: List[SrcCtx]) = metadata.add(x, SrcCtxs(ctx))
  }
  def mpos(pos: List[SrcCtx]) = pos.head
  def mpos(s: Exp[_]) = ctxsOf(s).head

  def ctxOrHere(x: Exp[_])(implicit ctx: SrcCtx): SrcCtx = ctxsOf(x).headOption.getOrElse(ctx)

  /** Any FStaged symbol **/
  sealed abstract class Exp[+T] private[core](bstaged: BStaged[T]) extends EdgeLike {
    def tp: BStaged[T @uncheckedVariance] = bstaged

    def addCtx(ctx: SrcCtx) { ctxsOf(this) = ctx +: ctxsOf(this) }
    def setCtx(ctx: SrcCtx) { ctxsOf(this) = List(ctx) }
  }

  /** A FStaged symbol which represents a non-constant value **/
  // TODO: This could use a name change
  sealed abstract class Symbol[+T] private[core](bstaged: BStaged[T]) extends Exp[T](bstaged) with Edge {
    override def hashCode(): Int = id
    override def equals(x: Any) = x match {
      case that: Symbol[_] => this.id == that.id
      case _ => false
    }

    def dependents: List[Exp[_]] = dependentsOf(this.id).flatMap(nodeOutputs).map(symFromSymId)
  }

  /** FStaged symbols created as bound variables **/
  class Bound[+T] private[core](bstaged: BStaged[T]) extends Symbol[T](bstaged) {
    override def toString = s"b$id"
  }

  /** FStaged symbols with definitions **/
  class Sym[+T] private[core](bstaged: BStaged[T]) extends Symbol[T](bstaged) {
    override def toString = s"x$id"
  }



  // In compiler API, we want to be specify FStaged constants:
  //   def foo(x: Const[Int]) ...
  // In compiler, we want to be able to write things like:
  //   case x: Param[_] => x.c = 3
  // and be guaranteed that this is legal
  // :: Param is a special, mutable case of Const
  /** A FStaged constant **/
  class Const[+T] private[core](x: Any)(bstaged: BStaged[T]) extends Exp[T](bstaged) {
    private val _c: Any = x
    def c: Any = _c

    override def hashCode() = (tp, c).hashCode()
    override def equals(x: Any) = x match {
      case that: Const[_] => this.tp == that.tp && this.c == that.c
      case _ => false
    }

    override def toString = escapeConst(c)
  }

  /** A FStaged, mutable constant **/
  private var nParams = 0

  class Param[+T] private[core](x: Any)(bstaged: BStaged[T]) extends Const[T](x)(bstaged) {
    private[argon] val pid = {nParams -= 1; nParams}

    private var _p: Any = x
    override def c: Any = _p
    def c_=(rhs: Any) { if (!isFinal) _p = rhs }
    override def toString = escapeConst(c)

    private var _isFinal: Boolean = false
    def isFinal: Boolean = _isFinal
    def makeFinal(): Unit = { _isFinal = true }

    override def hashCode() = pid
    override def equals(x: Any) = x match {
      case that: Param[_] => this.pid == that.pid
      case _ => false
    }
  }

  private[core] def __sym(tp: BStaged[_]): Sym[_] = new Sym(tp)
  private[core] def __bound[T:BStaged]: Bound[T] = new Bound(btyp[T])
  private[core] def __const[T:BStaged](x: Any): Const[T] = new Const(x)(btyp[T])
  private[core] def __param[T:BStaged](x: Any): Param[T] = new Param(x)(btyp[T])


  def constUnapply(s: Exp[_]): Option[Any] = s match {
    case param:Param[_] if param.isFinal => Some(param.c)
    case const:Const[_] => Some(const.c)
    case _ => None
  }
  def paramUnapply(s: Exp[_]): Option[Any] = s match {
    case param:Param[_] => Some(param.c) // TODO: Should this only return if isFinal is false?
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
    case p: Param[_] => s"Param(${escapeConst(p)})"
    case c: Const[_] => s"Const(${escapeConst(c)})"
    case s: SrcCtxs => mpos(s.pos).toString()
    case t: BStaged[_] =>
      val tArgs = if (t.typeArguments.nonEmpty)
        t.typeArguments.map(readable).mkString("[",",","]")
      else ""
      readable(t.stagedClass) + tArgs

    case _ => super.readable(x)
  }

  override def userReadable(x: Any): String = x match {
    case t: BStaged[_] =>
      val tArgs = if (t.typeArguments.nonEmpty)
        t.typeArguments.map(userReadable).mkString("[",",","]")
      else ""
      userReadable(t.stagedClass) + tArgs
    case _ => super.userReadable(x)
  }
}
