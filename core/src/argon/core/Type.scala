package argon.core

import ops._
import argon.lang.MetaAny

import scala.annotation.implicitNotFound

/** Type evidence for staged types **/
@implicitNotFound(msg = "Type ${T} is not a staged type. Try adding an explicit lift() call?")
abstract class Type[T](implicit val ev: T <:< MetaAny[T]) extends FrontendFacing with CompilerFacing {

  def fakeT: T = wrapped(null)
  val fake: MetaAny[T] = ev(fakeT)
  type Internal = fake.Internal

  // TODO: In the future we may want to refactor these two methods out
  def unwrapped(x: T): Exp[T] = ev(x).s
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
  override def toStringCompiler = {
    val tArgs = if (typeArguments.nonEmpty) typeArguments.map(t => c"$t").mkString("[",",","]") else ""
    c"$stagedClass$tArgs"
  }
  override def toStringFrontend = {
    val tArgs = if (typeArguments.nonEmpty) typeArguments.map(t => u"$t").mkString("[",",","]") else ""
    c"$stagedClass$tArgs"
  }
}
