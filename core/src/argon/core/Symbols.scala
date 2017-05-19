package argon.core

import ops._
import argon.graphs.{Edge, EdgeLike}
import argon.utils.escapeConst
import forge._

import scala.annotation.unchecked.uncheckedVariance

/** Any staged symbol **/
sealed abstract class Exp[+T] extends EdgeLike with FrontendFacing {
  def tp: Type[T @uncheckedVariance]

  var name: Option[String] = None
  override def toStringFrontend = name match {
    case Some(name) => name + " (" + this.toString + ")"
    case None => this.toString
  }
}

/** A staged symbol which represents a non-constant value **/
sealed abstract class Dyn[+T] extends Exp[T] with Edge {
  override def hashCode(): Int = id
  override def equals(x: Any) = x match {
    case that: Dyn[_] => this.id == that.id
    case _ => false
  }

  @stateful def dependents: Seq[Exp[_]] = {
    state.graph.dependentsOf(this.id).flatMap{dep => state.graph.nodeOutputs(dep)}.map(symFromSymId)
  }
}

/** Staged symbols created as bound variables **/
class Bound[+T](val tp: Type[T @ uncheckedVariance]) extends Dyn[T] {
  override def toString = s"b$id"
}

/** Staged symbols with definitions **/
class Sym[+T](val tp: Type[T @uncheckedVariance]) extends Dyn[T] {
  override def toString = s"x$id"
}

// TODO: Investigate re-adding T<:MetaAny[_] type bound to Const and Param, changing c to T#Internal
// In compiler API, we want to be specify staged constants:
//   def foo(x: Const[Int]) ...
// In compiler, we want to be able to write things like:
//   case x: Param[_] => x.c = 3
// and be guaranteed that this is legal
// :: Param is a special, mutable case of Const
/** A staged constant **/
class Const[+T](val tp: Type[T@uncheckedVariance])(x: Any) extends Exp[T] {
  private val _c: Any = x
  def c: Any = _c

  override def hashCode() = (tp, c).hashCode()
  override def equals(x: Any) = x match {
    case that: Const[_] => this.tp == that.tp && this.c == that.c
    case _ => false
  }

  override def toString = s"Const(${escapeConst(_c)})"
  override def toStringFrontend = escapeConst(_c)
}

/** A Staged, mutable constant **/
class Param[+T](override val tp: Type[T@uncheckedVariance])(val x: Any, val pid: Int) extends Const[T](tp)(x) {
  private var _c: Any = x
  override def c: Any = _c
  def c_=(rhs: Any): Unit = if (!isFinal) _c = rhs

  private var _isFinal: Boolean = false
  def isFinal: Boolean = _isFinal
  def makeFinal(): Unit = { _isFinal = true }

  override def hashCode() = pid
  override def equals(x: Any) = x match {
    case that: Param[_] => this.pid == that.pid
    case _ => false
  }

  override def toString = s"Param(#${-pid})"
  override def toStringFrontend = this.toString   // TODO: Is this what is we want here?
}

// TODO: Investigate why this still gives back Any even when T#Internal is used
/*object Lit {
  def unapply[T<:MetaAny[T]](s: Exp[T]): Option[T#Internal] = s match {
    case param: Param[_] if param.isFinal => Some(param.c)
    case const: Const[_] => Some(const.c)
    case _ => None
  }
}*/

object Const {
  def unapply(s: Exp[_]): Option[Any] = s match {
    case param: Param[_] if param.isFinal => Some(param.c)
    case const: Const[_] => Some(const.c)
    case _ => None
  }
}

object Param {
  def unapply(s: Exp[_]): Option[Any] = s match {
    case param: Param[_] => Some(param.c)
    case _ => None
  }
}
