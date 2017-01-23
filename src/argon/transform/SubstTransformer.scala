package argon.transform

trait SubstTransformer extends Transformer {
  import IR._

  private var subst: Map[Exp[_],Exp[_]] = Map.empty

  // Syntax is, e.g.: register(x -> y)
  // Technically original and replacement should have the same type, but this type currently can be "Any"
  def register[T](rule: (Exp[T], Exp[T])) = {
    assert(rule._1.tp == rule._2.tp)
    subst += rule
  }
  def remove[T](key: Exp[T]) = subst -= key

  protected def transformSym[T:Staged](s: Sym[T]): Exp[T] = subst.get(s) match {
    case Some(y) => y.asInstanceOf[Exp[T]]
    case None => s
  }

  def withSubstScope[A](extend: (Exp[Any],Exp[Any])*)(block: => A): A =
    withSubstScope {
      extend.foreach{case (x,y) => register(x -> y) }
      block
    }

  def withSubstScope[A](block: => A): A = {
    val save = subst
    val r = block
    subst = save
    r
  }
}
