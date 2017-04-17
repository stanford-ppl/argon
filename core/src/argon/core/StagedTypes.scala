package argon.core

import forge._

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import argon.State

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext
  type Bool
  type Text

  /** Type evidence for staged types **/
  @implicitNotFound(msg = "Type ${T} is not a staged type. Try adding an explicit lift() call?")
  abstract class Type[T](implicit val ev: T <:< MetaAny[T]) {
    // TODO: In the future we may want to refactor these two methods out
    def unwrapped(x: T): Exp[T] = x.s
    def wrapped(x: Exp[T]): T

    def typeArguments: List[Type[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean

    final def <:<(that: Type[_]) = isSubtype(this.stagedClass, that.stagedClass)

    /** Stolen from Delite utils **/
    private def isSubtype(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
      if ((x == cls) || x.getInterfaces.contains(cls)) true
      else if (x.getSuperclass == null && x.getInterfaces.length == 0) false
      else {
        val superIsSub = if (x.getSuperclass != null) isSubtype(x.getSuperclass, cls) else false
        superIsSub || x.getInterfaces.exists(s=>isSubtype(s,cls))
      }
    }
  }
  implicit def subTypeEv[T:Meta](x: T): MetaAny[T] = meta[T].ev(x)

  /** Frontend staged type evidence **/
  // TODO: Ideally Type and Meta these would be different, or somehow separated.
  /*abstract class Meta[T](implicit val ev: T <:< MetaAny[T]) extends Type[T] {
    def unwrapped(x: T): Exp[T] = x.s
    def wrapped(x: Exp[T]): T
  }*/
  type Meta[T] = Type[T]
 
  private def unstagedWarning(op: String)(implicit ctx: SrcCtx): Unit = {
    warn(ctx, s"Unstaged method $op was used here on a staged type during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }
  private def unstagedWarningNoCtx(op: String)(ctx: SrcCtx): Unit = {
    // val name = ctx.lhsName.getOrElse("the value")
    val name = "the value"
    warn(ctx, s"Unstaged method $op was used on $name defined here during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }


  /** Base trait for all staged, frontend types **/
  abstract class MetaAny[T:Meta] extends Product {
    def s: Exp[T]

    private def isEqual(that: Any): Boolean = that match {
      case x: MetaAny[_] => this.s == x.s
      case _ => false
    }

    /*final def ==(that: Any)(implicit ctx: SrcCtx): Boolean = {
      if (State.staging) unstagedWarning("==")
      this.isEqual(that)
    }
    final def !=(that: Any)(implicit ctx: SrcCtx): Boolean = {
      if (State.staging) unstagedWarning("!=")
      !this.isEqual(that)
    }
    final def toString(implicit ctx: SrcCtx): String = {
      if (State.staging) unstagedWarning("toString")
      this.productPrefix + this.productIterator.mkString("(", ", ", ")")
    }*/
    override def toString(): String = {
      if (State.staging) unstagedWarningNoCtx("toString()")(s.ctx)
      this.productPrefix + this.productIterator.mkString("(", ", ", ")")
    }

    override def equals(that: Any): Boolean = {
      if (State.staging) unstagedWarningNoCtx("equals")(s.ctx)
      this.isEqual(that)
    }

    @api def !=(that: T): Bool = this =!= that
    @api def ==(that: T): Bool = this === that
    @api def ===(that: T): Bool
    @api def =!=(that: T): Bool
    @api def toText: Text
  }

  def liftString(x: String)(implicit ctx: SrcCtx): Text
  def concat(x: Text, y: Text)(implicit ctx: SrcCtx): Text

  @util def infix_toString[T<:MetaAny[T]](x: T): Text = x.toText

  def __valDef[T<:MetaAny[T]](init: T, name: String): Unit = {
    log(c"Setting name of ${init.s} to $name")
    //init.s.ctx.lhsName = Some(name)
    nameOf(init.s) = name
  }

  @util def __equals[T<:MetaAny[T]](x: T, y: T): Bool = x === y
  @util def __equals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): Bool = lift(x) === y
  @util def __equals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): Bool = x === lift(y)
  @util def __unequals[T<:MetaAny[T]](x: T, y: T): Bool = x =!= y
  @util def __unequals[A, T<:MetaAny[T]](x: A, y: T)(implicit lift: Lift[A, T]): Bool = lift(x) =!= y
  @util def __unequals[A, T<:MetaAny[T]](x: T, y: A)(implicit lift: Lift[A, T]): Bool = x =!= lift(y)

  def typ[T:Type] = implicitly[Type[T]]
  def mtyp[A,B](x: Type[A]): Type[B] = x.asInstanceOf[Type[B]]

  def meta[T:Meta] = implicitly[Meta[T]]
  def mmeta[A,B](x: Meta[A]): Meta[B] = x.asInstanceOf[Meta[B]]

  def wrap[T:Meta](s: Exp[T]): T = meta[T].wrapped(s)
  def wrap[T:Meta](xs: List[Exp[T]]): List[T] = xs.map{t => meta[T].wrapped(t) }
  def wrap[T:Meta](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => meta[T].wrapped(t) }

  def unwrap[T:Meta](x: T): Exp[T] = meta[T].unwrapped(x)
  def unwrap[T:Meta](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => meta[T].unwrapped(t) }
  def unwrap[T:Meta](xs: List[T]): List[Exp[T]] = xs.map{t => meta[T].unwrapped(t) }

  import StagedTypes._
  //def infix_==[A,B](x1: MetaAny[A], x2: MetaAny[B]): Bool = macro equalImpl[Bool]
  //def infix_==[A, B, C <:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalLiftRightImpl[Bool]
  //def infix_==[A, B, C <:MetaAny[B]](x1: A, x2: MetaAny[B])(implicit l: Lift[A,C]): Bool = macro equalLiftLeftImpl[Bool]

  //def infix_!=[A, B](x1: MetaAny[A], x2: MetaAny[B]): Bool = macro unequalImpl[Bool]
  //def infix_!=[A, B, C<:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalLiftRightImpl[Bool]
  //def infix_!=[A, B, C<:MetaAny[B]](x1:A, x2: MetaAny[B])(implicit l: Lift[A,C]): Bool = macro unequalLiftLeftImpl[Bool]

  // TODO: Should these lifts be casts?
  def infix_==[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: B): Bool = macro equalImpl[Bool]
  def infix_==[A, B<:MetaAny[B]](x1: B, x2: A)(implicit l: Lift[A,B]): Bool = x1 === lift(x2)
  def infix_==[A, B<:MetaAny[B]](x1: A, x2: B)(implicit l: Lift[A,B]): Bool = lift(x1) === x2

  def infix_!=[A<:MetaAny[A], B<:MetaAny[B]](x1: A, x2: B): Bool = macro unequalImpl[Bool]
  def infix_!=[A, B<:MetaAny[B]](x1: B, x2: A)(implicit l: Lift[A,B]): Bool = x1 =!= lift(x2)
  def infix_!=[A, B<:MetaAny[B]](x1: A, x2: B)(implicit l: Lift[A,B]): Bool = lift(x1) =!= x2

  // TODO: Should casts be implicit or explicit? Should have subtypes?

  @implicitNotFound(msg = "Cannot find way to cast type ${A} to type ${B}.")
  abstract class Cast[A,B](implicit mB: Meta[B]) {
    val staged = mB
    def apply(x: A)(implicit ctx: SrcCtx): B
  }

  final def cast[A,B](x: A)(implicit ctx: SrcCtx, c: Cast[A,B]): B = c(x)

  /** Lift[A,B] is used in place of Type[T] for user-facing type parameters, where the user may either
    * give an unstaged constant or a staged symbol as the return value.
    *
    * NOTE: Including evidence of Type[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve Type[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result Type[_] in scope to qualify.
    **/
  @implicitNotFound(msg = "Cannot find way to lift type ${A} to type ${B}. Try adding an explicit cast using .to[${B}].")
  abstract class Lift[A,B](implicit mB: Meta[B]) {
    val staged = mB
    def apply(x: A)(implicit ctx: SrcCtx): B
  }

  final def lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l(x)

  implicit def selfLift[T:Meta]: Lift[T,T] = new Lift[T,T] {
    override def apply(x: T)(implicit ctx: SrcCtx): T = x
  }
}


private object StagedTypes {
  def equalImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === $x2")
  }

  def equalLiftRightImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 === lift($x2)")
  }

  def equalLiftLeftImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === $x2")
  }

  def unequalImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    c.Expr(q"__unequals($x1, $x2)")
  }

  def unequalLiftRightImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"$x1 =!= lift($x2)")
  }

  def unequalLiftLeftImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) =!= $x2")
  }
}
