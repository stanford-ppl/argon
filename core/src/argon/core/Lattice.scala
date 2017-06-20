package argon.core

/**
  *          Top (⊤)
  *
  *         Join(X,Y)
  *          /    \
  *         X     Y
  *         \    /
  *        Meet(X,Y)
  *
  *        Bottom (⊥)
  */
trait Lattice[T] {
  def meet(a: T, b: T): T
  def join(a: T, b: T): T

  val top: Option[T]
  val bottom: Option[T]

  /**
    *  On metadata meet, behavior of meet(Option[T], Option[T]) is:
    *  true:  On alias, prefer Some over None. On init, prefer Some. On update, prefer most recent.
    *  false: On alias, prefer None over Some. On init, prefer Some. On update, prefer most recent.
    */
  def isEmpiric(a:T): Boolean
}