package argon.core

import scala.annotation.implicitNotFound
import org.virtualized.{EmbeddedControls, SourceContext}
import argon.State

trait StagedTypes extends EmbeddedControls { this: Staging =>
  type SrcCtx = SourceContext

  /** Base type class for all FStaged types **/
  trait FStaged[T <: StageAny[T]] extends BStaged[T]{
    def wrapped(x: Exp[T]): T
  }

  trait BStaged[T] {
    def typeArguments: List[BStaged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
    def <:<(that: FStaged[_]) = isSubtype(this.stagedClass, that.stagedClass)
  }

  /*  def toBStaged[T](s: FStaged[T]):BStaged[T] = new BStaged[T] {
  override def typeArguments: List[BStaged[_]] = s.typeArguments.map(x => toBStaged(x))
  override def stagedClass = s.stagedClass

  override def isPrimitive = s.isPrimitive
}
*/

  trait StageAny[+T] {
    def s: Exp[T]
  }


  def ftyp[T <: StageAny[T] : FStaged]: FStaged[T] = implicitly[FStaged[T]]
  def btyp[T:BStaged] = implicitly[BStaged[T]]
  def mbtyp[A,B](x: BStaged[A]): BStaged[B] = x.asInstanceOf[BStaged[B]]

  def wrap[T <: StageAny[T] : FStaged](s: Exp[T]): T = implicitly[FStaged[T]].wrapped(s)
  def wrap[T <: StageAny[T] : FStaged](xs: List[Exp[T]]): List[T] = xs.map{t => implicitly[FStaged[T]].wrapped(t) }
  def wrap[T <: StageAny[T] : FStaged](xs: Seq[Exp[T]]): Seq[T] = xs.map{t => implicitly[FStaged[T]].wrapped(t) }


  /** Stolen from Delite utils **/
  private def isSubtype(x: java.lang.Class[_], cls: java.lang.Class[_]): Boolean = {
    if ((x == cls) || x.getInterfaces.contains(cls)) true
    else if (x.getSuperclass == null && x.getInterfaces.length == 0) false
    else {
      val superIsSub = if (x.getSuperclass != null) isSubtype(x.getSuperclass, cls) else false
      superIsSub || x.getInterfaces.exists(s=>isSubtype(s,cls))
    }
  }


  /** Lift[A,B] is used in place of FStaged[T] for user-facing type parameters, where the user may either
    * give an unFStaged constant or a FStaged symbol as the return value.
    *
    * NOTE: Including evidence of FStaged[B] as an implicit parameter to Lift instances leads to problems with implicit
    * ambiguity when calling lift(x), since the compiler may attempt to resolve FStaged[B] before it resolves Lift[A,B],
    * causing any implicit value or def with result FStaged[_] in scope to qualify.
    **/

  @implicitNotFound(msg = "Cannot find way to lift type ${A}. Try adding explicit lift(_) calls to return value(s).")
  trait Lift[A,B <: StageAny[B]] {
    def fStaged: FStaged[B]
    def lift(x: A)(implicit ctx: SrcCtx): B = __lift(x)(ctx, this)
  }

  def __lift[A,B <: StageAny[B]](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B
  final def lift[A,B <: StageAny[B]](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.lift(x)

  implicit def selfLift[T <: StageAny[T] : FStaged]: Lift[T,T] = new Lift[T,T] {
    def fStaged = implicitly[FStaged[T]]
    override def lift(x: T)(implicit ctx: SrcCtx): T = x
  }



  def reset(): Unit = {
    State.flex = false
    State.staging = false
    State.EVAL = false
    State.pass = 1
  }
}
