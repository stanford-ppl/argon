package argon

import argon.core.UserFacing
import argon.graphs.{Edge, EdgeLike}
import argon.utils.escapeConst
import forge._

import scala.annotation.unchecked.uncheckedVariance

/** Any staged symbol **/
sealed abstract class Exp[+T] extends EdgeLike with UserFacing {
  def tp: Type[T @uncheckedVariance]

  var name: Option[String] = None
  override def toStringUser = name match {
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

// In compiler API, we want to be specify staged constants:
//   def foo(x: Const[Int]) ...
// In compiler, we want to be able to write things like:
//   case x: Param[_] => x.c = 3
// and be guaranteed that this is legal
// :: Param is a special, mutable case of Const
/** A staged constant **/
class Const[+T<:MetaAny[_]](val tp: Type[T@uncheckedVariance])(x: T#Internal) extends Exp[T] {
  private val _c: T#Internal = x
  @stateful def c(implicit state: State): T#Internal = _c // Not actually stateful, but Param's method is.

  override def hashCode() = (tp, c).hashCode()
  override def equals(x: Any) = x match {
    case that: Const[_] => this.tp == that.tp && this.c == that.c
    case _ => false
  }

  override def toString = s"Const(${escapeConst(c)})"
  override def toStringUser = escapeConst(c)
}

/** A Staged, mutable constant **/
case class ParamValue(value: Any) extends Metadata[ParamValue] { def mirror(f: Tx) = this }

class Param[+T<:MetaAny[_]](override val tp: Type[T@uncheckedVariance])(x: T#Internal, pid: Int) extends Const[T](tp)(x) {
  @stateful override def c(implicit state: State): T#Internal = {
    state.metadata[ParamValue](this).map(_.value.asInstanceOf[T#Internal]).getOrElse(x)
  }
  @stateful def c_=(rhs: T#Internal)(implicit state: State): Unit = {
    if (!isFinal) state.metadata.add(this, ParamValue(rhs))
  }

  private var _isFinal: Boolean = false
  def isFinal: Boolean = _isFinal
  def makeFinal(): Unit = { _isFinal = true }

  override def hashCode() = pid
  override def equals(x: Any) = x match {
    case that: Param[_] => this.pid == that.pid
    case _ => false
  }

  override def toString = s"Param(#${-pid})"
  override def toStringUser = this.toString   // TODO: Is this what is we want here?
}

object Const {
  @stateful def unapply[T<:Any](s: Exp[T]): Option[Any] = s match {
    case param: Param[_] if param.isFinal => Some(param.c)
    case const: Const[_] => Some(const.c)
    case _ => None
  }
  @stateful def unapply[T<:MetaAny[T]](s: Exp[T]): Option[T#Internal] = s match {
    case param: Param[_] if param.isFinal => Some(param.c)
    case const: Const[_] => Some(const.c)
    case _ => None
  }
}

object Param {
  @stateful def unapply[T<:MetaAny[T]](s: Exp[T]): Option[T#Internal] = s match {
    // TODO: Should this only return if isFinal is false?
    case param:Param[_] => Some(param.c)
    case _ => None
  }
  @stateful def unapply[T<:Any](s: Exp[T]): Option[Any] = s match {
    case param: Param[_] => Some(param.c)
    case _ => None
  }
}
