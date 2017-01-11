package argon.core

trait StagedTypes { this: Staging =>

  /** Base type class for all staged types **/
  abstract class Staged[T] {
    def wrapped(x: Sym[T]): T
    def unwrapped(x: T): Sym[T]
    def typeArguments: List[Staged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
  }

  def typ[T:Staged] = implicitly[Staged[T]]
  def mtyp[A,B](x: Staged[A]): Staged[B] = x.asInstanceOf[Staged[B]]

  def wrap[T:Staged](s: Sym[T]): T = implicitly[Staged[T]].wrapped(s)
  def unwrap[T:Staged](x: T): Sym[T] = implicitly[Staged[T]].unwrapped(x)

  def wrap[T:Staged](xs: List[Sym[T]]): List[T] = xs.map{t => implicitly[Staged[T]].wrapped(t) }
  def unwrap[T:Staged](xs: List[T]): List[Sym[T]] = xs.map{t => implicitly[Staged[T]].unwrapped(t) }
  def wrap[T:Staged](xs: Seq[Sym[T]]): Seq[T] = xs.map{t => implicitly[Staged[T]].wrapped(t) }
  def unwrap[T:Staged](xs: Seq[T]): Seq[Sym[T]] = xs.map{t => implicitly[Staged[T]].unwrapped(t) }

  implicit class StagedTypeOps[T:Staged](x: T) {
    def s: Sym[T] = implicitly[Staged[T]].unwrapped(x)
  }
}
