package argon.core

import argon.graphs.{Edge, EdgeLike, Graph}
import argon.utils.escapeConst
import org.virtualized.EmptyContext

import scala.annotation.unchecked.uncheckedVariance


trait Symbols extends StagedTypes with Metadata with Graph { self: Staging =>

  case class SrcCtxs(pos: List[SrcCtx]) extends Metadata[SrcCtxs] { def mirror(f:Tx) = this }
  object ctxsOf {
    def apply(x: Exp[_]): List[SrcCtx] = metadata[SrcCtxs](x).map(_.pos).getOrElse(Nil)
    def update(x: Exp[_], ctx: List[SrcCtx]) = metadata.add(x, SrcCtxs(ctx))
  }
  def mpos(pos: List[SrcCtx]) = pos.head
  def mpos(s: Exp[_]) = ctxsOf(s).head

  implicit class SrcCtxOps(x: Exp[_]) {
    def ctx: SrcCtx = ctxsOf(x).headOption.getOrElse(EmptyContext)
    def ctxOrElse(els: SrcCtx) = ctxsOf(x).headOption.getOrElse(els)
    def addCtx(ctx: SrcCtx) { ctxsOf(x) = ctx +: ctxsOf(x) }
    def setCtx(ctx: SrcCtx) { ctxsOf(x) = List(ctx) }
  }

  object nameOf {
    def apply(x: Exp[_]): Option[String] = ctxsOf(x).find(_.lhsName.isDefined).flatMap(_.lhsName)
  }

  def ctx(implicit context: SrcCtx): SrcCtx = context


  /** Any staged symbol **/
  sealed abstract class Exp[+T] private[core](staged: Type[T]) extends EdgeLike {
    def tp: Type[T @uncheckedVariance] = staged
  }

  /** A staged symbol which represents a non-constant value **/
  sealed abstract class Dyn[+T] private[core](staged: Type[T]) extends Exp[T](staged) with Edge {
    override def hashCode(): Int = id
    override def equals(x: Any) = x match {
      case that: Dyn[_] => this.id == that.id
      case _ => false
    }

    def dependents: Seq[Exp[_]] = dependentsOf(this.id).flatMap(nodeOutputs).map(symFromSymId)
  }

  /** Staged symbols created as bound variables **/
  class Bound[+T] private[core](staged: Type[T]) extends Dyn[T](staged) {
    override def toString = s"b$id"
  }

  /** Staged symbols with definitions **/
  class Sym[+T] private[core](staged: Type[T]) extends Dyn[T](staged) {
    override def toString = s"x$id"
  }

  // In compiler API, we want to be specify staged constants:
  //   def foo(x: Const[Int]) ...
  // In compiler, we want to be able to write things like:
  //   case x: Param[_] => x.c = 3
  // and be guaranteed that this is legal
  // :: Param is a special, mutable case of Const
  /** A staged constant **/
  class Const[+T] private[core](x: Any)(staged: Type[T]) extends Exp[T](staged) {
    private val _c: Any = x
    def c: Any = _c

    override def hashCode() = (tp, c).hashCode()
    override def equals(x: Any) = x match {
      case that: Const[_] => this.tp == that.tp && this.c == that.c
      case _ => false
    }

    override def toString = escapeConst(c)
  }

  /** A Staged, mutable constant **/
  private var nParams = 0

  class Param[+T] private[core](x: Any)(staged: Type[T]) extends Const[T](x)(staged) {
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

  object Const {
    def unapply(s: Exp[_]): Option[Any] = s match {
      case param:Param[_] if param.isFinal => Some(param.c)
      case const:Const[_] => Some(const.c)
      case _ => None
    }
  }

  object Param {
    def unapply(s: Exp[_]): Option[Any] = s match {
      case param:Param[_] => Some(param.c) // TODO: Should this only return if isFinal is false?
      case _ => None
    }
  }

  /** Compiler debugging **/
  override def readable(x: Any): String = x match {
    case s: Sym[_] => s"x${s.id}"
    case b: Bound[_] => s"b${b.id}"
    case p: Param[_] => s"Param(${escapeConst(p)})"
    case c: Const[_] => s"Const(${escapeConst(c)})"
    case s: SrcCtxs => mpos(s.pos).toString()
    case t: Type[_] =>
      val tArgs = if (t.typeArguments.nonEmpty)
        t.typeArguments.map(readable).mkString("[",",","]")
      else ""
      readable(t.stagedClass) + tArgs

    case _ => super.readable(x)
  }

  override def userReadable(x: Any): String = x match {
    case e: Exp[_] => nameOf(e) match {
      case Some(name) => name + " (" + super.userReadable(e) + ")"
      case None => super.userReadable(e)
    }
    case t: Type[_] =>
      val tArgs = if (t.typeArguments.nonEmpty)
        t.typeArguments.map(userReadable).mkString("[",",","]")
      else ""
      userReadable(t.stagedClass) + tArgs
    case _ => super.userReadable(x)
  }
}
