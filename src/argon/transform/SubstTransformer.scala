package argon.transform

trait SubstTransformer extends Transformer {
  import IR._

  var subst: Map[Exp[_],Exp[_]] = Map.empty

  // Syntax is, e.g.: register(x -> y)
  // Technically original and replacement should have the same type, but this type currently can be "Any"
  def register[T](rule: (Exp[T], Exp[T])) = {
    assert(rule._1.tp == rule._2.tp, c"When creating substitution ${rule._1} -> ${rule._2}, type ${rule._1.tp} does not match ${rule._2.tp}")
    subst += rule
  }
  def remove[T](key: Exp[T]) = subst -= key

  override protected def transformExp[T:BStaged](s: Exp[T]): Exp[T] = subst.get(s) match {
    case Some(y) => y.asInstanceOf[Exp[T]]
    case None => s
  }

  def withSubstScope[A](extend: (Exp[Any],Exp[Any])*)(block: => A): A =
    isolateSubstScope {
      extend.foreach{rule => register(rule) }
      block
    }

  def isolateSubstScope[A](block: => A): A = {
    val save = subst
    val r = block
    subst = save
    r
  }

  def withSubstRules[A](rules: Map[Exp[_],Exp[_]])(block: => A): A = {
    val save = subst
    subst ++= rules
    val result = block
    subst = save
    result
  }
}
