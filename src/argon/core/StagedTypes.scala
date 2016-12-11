package argon.core

trait StagedTypes { this: Staging =>

  abstract class Staged[T] {
    def wrap(x: Sym[T]): T
    def unwrap(x: T): Sym[T]
    def typeArguments: List[Staged[_]] = Nil
    def stagedClass: Class[T]
    def isPrimitive: Boolean
  }

  def stg[T:Staged] = implicitly[Staged[T]]
  def mstg[A,B](x: Staged[A]): Staged[B] = x.asInstanceOf[Staged[B]]

  def wrap[T:Staged](s: Sym[T]): T = implicitly[Staged[T]].wrap(s)
  def unwrap[T:Staged](x: T): Sym[T] = implicitly[Staged[T]].unwrap(x)

  def wrap[T:Staged](xs: List[Sym[T]]): List[T] = xs.map{t => implicitly[Staged[T]].wrap(t) }
  def unwrap[T:Staged](xs: List[T]): List[Sym[T]] = xs.map{t => implicitly[Staged[T]].unwrap(t) }
  def wrap[T:Staged](xs: Seq[Sym[T]]): Seq[T] = xs.map{t => implicitly[Staged[T]].wrap(t) }
  def unwrap[T:Staged](xs: Seq[T]): Seq[Sym[T]] = xs.map{t => implicitly[Staged[T]].unwrap(t) }

  implicit class StagedTypeOps[T:Staged](x: T) {
    def s: Sym[T] = implicitly[Staged[T]].unwrap(x)
  }
}
