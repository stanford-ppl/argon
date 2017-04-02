package argon.core

import forge._

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext
  type Bool
  type Text

  /** Base type class for all staged types **/
  @implicitNotFound(msg = "Type ${T} is not a staged type. Try adding an explicit lift() call?")
  abstract class Type[T](implicit val ev: T <:< MetaAny[T]) {
    def unwrapped(x: T): Exp[T] = x.s
    def wrapped(x: Exp[T]): T
    def typeArguments: List[Type[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean

    def <:<(that: Type[_]) = isSubtype(this.stagedClass, that.stagedClass)

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

  type Meta[T] = Type[T]
  //abstract class Meta[T] extends Type[T]

  // Has to be an implicit class to not conflict with higher priority implicits on +
  implicit class ConcatOps[T<:MetaAny[T]](lhs: T) {
    @api def +(rhs: String): Text = concat(lhs.toText, liftString(rhs))
    @api def +(rhs: Text): Text = concat(lhs.toText, rhs)
    @api def +[R](rhs: MetaAny[R]): Text = concat(lhs.toText, rhs.toText)
  }

  /** Base trait for all staged, frontend types **/
  abstract class MetaAny[T:Meta] {
    def s: Exp[T]
    @api def !=(that: T): Bool = this =!= that
    @api def ==(that: T): Bool = this === that
    @api def ===(that: T): Bool
    @api def =!=(that: T): Bool
    @api def toText: Text
  }

  def liftString(x: String)(implicit ctx: SrcCtx): Text
  def concat(x: Text, y: Text)(implicit ctx: SrcCtx): Text

  @util def infix_toString[T<:MetaAny[T]](x: T) = x.toText
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

  def wrap[T:Type](s: Exp[T]): T = typ[T].wrapped(s)
  def wrap[T:Type](xs: List[Exp[T]]): List[T] = xs.map{t => typ[T].wrapped(t) }
  def wrap[T:Type](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => typ[T].wrapped(t) }

  def unwrap[T:Meta](x: T): Exp[T] = meta[T].unwrapped(x)
  def unwrap[T:Meta](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => meta[T].unwrapped(t) }
  def unwrap[T:Meta](xs: List[T]): List[Exp[T]] = xs.map{t => meta[T].unwrapped(t) }

  import StagedTypes._
  def infix_==[A,B](x1: MetaAny[A], x2: MetaAny[B]): Bool = macro equalImpl[Bool]
  def infix_==[A, B, C <:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalLiftRightImpl[Bool]
  def infix_==[A, B, C <:MetaAny[B]](x1: A, x2: MetaAny[B])(implicit l: Lift[A,C]): Bool = macro equalLiftLeftImpl[Bool]

  def infix_!=[A, B](x1: MetaAny[A], x2: MetaAny[B]): Bool = macro unequalImpl[Bool]
  def infix_!=[A, B, C<:MetaAny[B]](x1: MetaAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalLiftRightImpl[Bool]
  def infix_!=[A, B, C<:MetaAny[B]](x1:A, x2: MetaAny[B])(implicit l: Lift[A,C]): Bool = macro unequalLiftLeftImpl[Bool]


  /** Lift[A,B] is used in place of Type[T] for user-facing type parameters, where the user may either
    * give an unStaged constant or a Staged symbol as the return value.
    *
    * NOTE: Including evidence of Type[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve Type[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result Type[_] in scope to qualify.
    **/
  @implicitNotFound(msg = "Cannot find way to cast type ${A} to type ${B}.")
  abstract class Cast[A,B](implicit mB: Meta[B]) {
    val staged = mB
    def apply(x: A)(implicit ctx: SrcCtx): B
  }

  @implicitNotFound(msg = "Cannot find way to lift type ${A} to type ${B}.")
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
    c.Expr(q"$x1 === lift($x2)")
  }

  def unequalLiftLeftImpl[T](c: whitebox.Context)(x1: c.Expr[Any], x2: c.Expr[Any])(l: c.Tree): c.Expr[T] = {
    import c.universe._
    c.Expr(q"lift($x1) === $x2")
  }
}