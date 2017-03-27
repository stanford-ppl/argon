package argon.core

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext
  type Bool
  type Text

  /** Base type class for all staged types **/
  sealed abstract class Type[T] {
    def wrapped(x: Exp[T]): T
    def unwrapped(x: T): Exp[T]
    def typeArguments: List[Type[_]] = Nil
    def stagedClass: Class[T] = classOf[T]
    def isPrimitive: Boolean

    def <:<(that: Type[_]) = isSubtype(this.stagedClass, that.stagedClass)
  }

  trait StagedAny[T] {
    def s: Exp[T]
    def ===(x: T)(implicit ctx: SrcCtx): Bool
    def =!=(x: T)(implicit ctx: SrcCtx): Bool
    def toText(implicit ctx: SrcCtx): Text
  }

  def infix_toString[T<:StagedAny[T]](x: T) = x.toText
  def __equals[T<:StagedAny[T]](x: T, y: T)(implicit ctx: SrcCtx): Bool = x === y
  def __equals[A, T<:StagedAny[T]](x: A, y: T)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = lift(x) === y
  def __equals[A, T<:StagedAny[T]](x: T, y: A)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = x === lift(y)
  def __unequals[T<:StagedAny[T]](x: T, y: T)(implicit ctx: SrcCtx): Bool = x =!= y
  def __unequals[A, T<:StagedAny[T]](x: A, y: T)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = lift(x) =!= y
  def __unequals[A, T<:StagedAny[T]](x: T, y: A)(implicit ctx: SrcCtx, l: Lift[A, T]): Bool = x =!= lift(y)

  import StagedTypes._


  def infix_==[A,B](x1: StageAny[A], x2: StageAny[B]): Bool = macro equalImpl[Bool]
  def infix_==[A, B, C <: StageAny[B]](x1: StageAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro equalLiftRightImpl[Bool]
  def infix_==[A, B, C <: StageAny[B]](x1: A, x2: StageAny[B])(implicit l: Lift[A,C]): Bool = macro equalLiftLeftImpl[Bool]

  def infix_!=[A, B](x1: StageAny[A], x2: StageAny[B]): Bool = macro unequalImpl[Bool]
  def infix_!=[A, B, C <: StageAny[B]](x1: StageAny[B], x2: A)(implicit l: Lift[A,C]): Bool = macro unequalLiftRightImpl[Bool]
  def infix_!=[A, B, C <: StageAny[B]](x1:A, x2: StageAny[B])(implicit l: Lift[A,C]): Bool = macro unequalLiftLeftImpl[Bool]


  abstract class Meta[T] extends Type[T] {

  }

  def typ[T:Type] = implicitly[Type[T]]
  def mtyp[A,B](x: Type[A]): Type[B] = x.asInstanceOf[Type[B]]

  def wrap[T:Type](s: Exp[T]): T = implicitly[Type[T]].wrapped(s)
  def unwrap[T:Type](x: T): Exp[T] = implicitly[Type[T]].unwrapped(x)

  def wrap[T:Type](xs: List[Exp[T]]): List[T] = xs.map{t => implicitly[Type[T]].wrapped(t) }
  def unwrap[T:Type](xs: List[T]): List[Exp[T]] = xs.map{t => implicitly[Type[T]].unwrapped(t) }
  def wrap[T:Type](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => implicitly[Type[T]].wrapped(t) }
  def unwrap[T:Type](xs: Seq[T]): Seq[Exp[T]] = xs.map{t => implicitly[Type[T]].unwrapped(t) }

  implicit class StagedTypeOps[T:Type](x: T) {
    def s: Exp[T] = implicitly[Type[T]].unwrapped(x)
  }

  /** Stolen from Delite utils **/
  private def isSubtype(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
    if ((x == cls) || x.getInterfaces.contains(cls)) true
    else if (x.getSuperclass == null && x.getInterfaces.length == 0) false
    else {
      val superIsSub = if (x.getSuperclass != null) isSubtype(x.getSuperclass, cls) else false
      superIsSub || x.getInterfaces.exists(s=>isSubtype(s,cls))
    }
  }


  /** Lift[A,B] is used in place of Staged[T] for user-facing type parameters, where the user may either
    * give an unstaged constant or a staged symbol as the return value.
    *
    * NOTE: Including evidence of Staged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve Staged[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result Staged[_] in scope to qualify.
    **/

  @implicitNotFound(msg = "Cannot find way to lift type ${A}. Try adding explicit lift(_) calls to return value(s).")
  trait Lift[A,B] {
    val staged: Type[B]
    def apply(x: A)(implicit ctx: SrcCtx): B
  }

  final def lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l(x)

  implicit def selfLift[T:Type]: Lift[T,T] = new Lift[T,T] {
    val staged = implicitly[Type[T]]
    override def apply(x: T)(implicit ctx: SrcCtx): T = x
  }

}
