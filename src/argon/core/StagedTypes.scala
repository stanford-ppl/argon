package argon.core

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext
  type Bool
  type Text

  /** Base type class for all staged types **/
  sealed trait Type[T] {
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

  /** Base trait for all staged, frontend types **/
  abstract class MetaAny[T:Meta] {
    def s: Exp[T]
    def ===(x: T)(implicit ctx: SrcCtx): Bool
    def =!=(x: T)(implicit ctx: SrcCtx): Bool
    def toText(implicit ctx: SrcCtx): Text
  }

  def infix_toString[T<:MetaAny[T]](x: T) = x.toText
  def __equals[T<:MetaAny[T]](x: T, y: T)(implicit ctx: SrcCtx): Bool = x === y
  def __equals[A, T<:MetaAny[T]](x: A, y: T)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = lift(x) === y
  def __equals[A, T<:MetaAny[T]](x: T, y: A)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = x === lift(y)
  def __unequals[T<:MetaAny[T]](x: T, y: T)(implicit ctx: SrcCtx): Bool = x =!= y
  def __unequals[A, T<:MetaAny[T]](x: A, y: T)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = lift(x) =!= y
  def __unequals[A, T<:MetaAny[T]](x: T, y: A)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = x =!= lift(y)

  @implicitNotFound(msg = "Type ${A} is not a staged type. Try adding an explicit lift() call?")
  abstract class Meta[T](implicit val ev: T <:< MetaAny[T]) extends Type[T] {
    def unwrapped(x: T): Exp[T] = x.s
  }
  implicit def subTypeEv[T:Meta](x: T): MetaAny[T] = meta[T].ev(x)

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


  /** Lift[A,B] is used in place of Staged[T] for user-facing type parameters, where the user may either
    * give an unStaged constant or a Staged symbol as the return value.
    *
    * NOTE: Including evidence of Staged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve Staged[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result Staged[_] in scope to qualify.
    **/

  @implicitNotFound(msg = "Cannot find way to lift type ${A} to type ${B}.")
  trait Lift[A,B] {
    val staged: Meta[B]
    def apply(x: A)(implicit ctx: SrcCtx): B
  }

  final def lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l(x)

  implicit def selfLift[T:Type]: Lift[T,T] = new Lift[T,T] {
    val staged = implicitly[Type[T]]
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
