package argon.transform

import argon.core.compiler._

trait SubstTransformer extends Transformer {
  var subst: Map[Exp[_],Exp[_]] = Map.empty

  // Syntax is, e.g.: register(x -> y)
  // Technically original and replacement should have the same type, but this type currently can be "Any"
  def register[T](rule: (Exp[T], Exp[T])) = {
    assert(rule._2.tp <:< rule._1.tp, c"When creating substitution ${rule._1} -> ${rule._2}, type ${rule._1.tp} was not a subtype of ${rule._2.tp}")
    subst += rule
  }
  def remove[T](key: Exp[T]) = subst -= key

  def registerOrRemove[T](rule: (Exp[T], Option[Exp[T]])) = rule._2 match {
    case Some(s) => register(rule._1,s)
    case None => remove(rule._1)
  }

  def getSubst[T](key: Exp[T]): Option[Exp[T]] = subst.get(key).asInstanceOf[Option[Exp[T]]]

  override protected def inlineLambda[A,R:Type](lambda1: Lambda1[A,R]) = {a: Exp[A] =>
    val prevA = getSubst(lambda1.input)
    register(lambda1.input -> a)
    val block = inlineBlock(lambda1)
    val result = block()
    registerOrRemove(lambda1.input -> prevA)
    result
  }
  override protected def inlineLambda[A,B,R:Type](lambda2: Lambda2[A,B,R]) = { (a: Exp[A],b: Exp[B]) =>
    val prevA = getSubst(lambda2.inputA)
    val prevB = getSubst(lambda2.inputB)
    register(lambda2.inputA -> a)
    register(lambda2.inputB -> b)
    val block = inlineBlock(lambda2)
    val result = block()
    registerOrRemove(lambda2.inputA -> prevA)
    registerOrRemove(lambda2.inputB -> prevB)
    result
  }
  override protected def inlineLambda[A,B,C,R:Type](lambda3: Lambda3[A,B,C,R]) = { (a: Exp[A], b: Exp[B], c: Exp[C]) =>
    val prevA = getSubst(lambda3.inputA)
    val prevB = getSubst(lambda3.inputB)
    val prevC = getSubst(lambda3.inputC)
    register(lambda3.inputA -> a)
    register(lambda3.inputB -> b)
    register(lambda3.inputC -> c)
    val block = inlineBlock(lambda3)
    val result = block()
    registerOrRemove(lambda3.inputA -> prevA)
    registerOrRemove(lambda3.inputB -> prevB)
    registerOrRemove(lambda3.inputC -> prevC)
    result
  }
  override protected def inlineLambda[A,B,C,D,R:Type](lambda4: Lambda4[A,B,C,D,R]) = { (a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]) =>
    val prevA = getSubst(lambda4.inputA)
    val prevB = getSubst(lambda4.inputB)
    val prevC = getSubst(lambda4.inputC)
    val prevD = getSubst(lambda4.inputD)
    register(lambda4.inputA -> a)
    register(lambda4.inputB -> b)
    register(lambda4.inputC -> c)
    register(lambda4.inputD -> d)
    val block = inlineBlock(lambda4)
    val result = block()
    registerOrRemove(lambda4.inputA -> prevA)
    registerOrRemove(lambda4.inputB -> prevB)
    registerOrRemove(lambda4.inputC -> prevC)
    registerOrRemove(lambda4.inputD -> prevD)
    result
  }

  override protected def transformExp[T:Type](s: Exp[T]): Exp[T] = subst.get(s) match {
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
  override def mirror(lhs: Seq[Sym[_]], rhs: Def) = {
    if (Config.verbosity > 2) {
      for((k,v) <- subst) {
        log(c"$k -> $v")
      }
    }
    super.mirror(lhs, rhs)
  }
}
