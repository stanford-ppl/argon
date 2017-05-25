package argon.core
package cake

trait LayerLattices {

  def lat[T:Lattice] = implicitly[Lattice[T]]
  def meet[T:Lattice](a: T, b: T): T = lat[T].meet(a, b)
  def join[T:Lattice](a: T, b: T): T = lat[T].join(a, b)

  implicit def OptionCanHaveLattice[T:Lattice]: Lattice[Option[T]] = new Lattice[Option[T]] {
    def meet(a: Option[T], b: Option[T]): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) => Some(lat[T].meet(am, bm))
      case (Some(am), None)     => None
      case (None, Some(bm))     => None
      case (None, None)         => None
    }
    def join(a: Option[T], b: Option[T]): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) => Some(lat[T].join(am, bm))
      case (Some(am), None)     => Some(am)
      case (None, Some(bm))     => Some(bm)
      case (None, None)         => None
    }

    val top    = None
    val bottom = Some(None)

    def isEmpiric(a:Option[T]): Boolean = a.forall{lat[T].isEmpiric(_)}
  }

  /*def metaAlias[T:Meetable](a: T, b: T) = meetable[T].meet(a,b)(MeetAlias)
  def metaInit[T:Meetable](a: T, default: T) = meetable[T].meet(a,default)(MeetInit)
  def metaUpdate[T:Meetable](a: T, prev: T) = meetable[T].meet(a,prev)(MeetUpdate)*/
}
