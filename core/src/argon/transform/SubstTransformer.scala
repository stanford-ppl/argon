package argon.transform

import argon.core._

trait SubstTransformer extends Transformer {
  val allowUnsafeSubst: Boolean = false

  var subst: Map[Exp[_],Exp[_]] = Map.empty

  // Syntax is, e.g.: register(x -> y)
  // Technically original and replacement should have the same type, but this type currently can be "Any"
  def register[T](rule: (Exp[T], Exp[T])) = {
    if (!allowUnsafeSubst) {
      assert(rule._2.tp <:< rule._1.tp, c"When creating substitution ${rule._1} -> ${rule._2}, type ${rule._2.tp} was not a subtype of ${rule._1.tp}")
    }
    subst += rule
  }
  // Only use if you know what you're doing!
  def registerUnsafe[A,B](rule: (Exp[A],Exp[B])): Unit = {
    subst += rule
  }
  def remove[T](key: Exp[T]) = subst -= key

  def registerOrRemove[T](rule: (Exp[T], Option[Exp[T]])) = rule._2 match {
    case Some(s) => register(rule._1,s)
    case None => remove(rule._1)
  }

  def getSubst[T](key: Exp[T]): Option[Exp[T]] = subst.get(key).asInstanceOf[Option[Exp[T]]]

  final override protected def blockToFunction0[R](b: Block[R], copy: Boolean): () => Exp[R] = isolateIf(copy){
    () => inlineBlock(b)
  }
  final override protected def lambda1ToFunction1[A,R](lambda1: Lambda1[A,R], copy: Boolean) = {a: Exp[A] => isolateIf(copy) {
    register(lambda1.input -> a)
    val block = blockToFunction0(lambda1, copy)
    block()
  }}
  final override protected def lambda2ToFunction2[A,B,R](lambda2: Lambda2[A,B,R], copy: Boolean) = { (a: Exp[A], b: Exp[B]) =>
    isolateIf(copy) {
      register(lambda2.inputA -> a)
      register(lambda2.inputB -> b)
      val block = blockToFunction0(lambda2, copy)
      block()
    }
  }
  final override protected def lambda3ToFunction3[A,B,C,R](lambda3: Lambda3[A,B,C,R], copy: Boolean) = { (a: Exp[A], b: Exp[B], c: Exp[C]) =>
    isolateIf(copy) {
      register(lambda3.inputA -> a)
      register(lambda3.inputB -> b)
      register(lambda3.inputC -> c)
      val block = blockToFunction0(lambda3, copy)
      block()
    }
  }
  final override protected def lambda4ToFunction4[A,B,C,D,R](lambda4: Lambda4[A,B,C,D,R], copy: Boolean) = { (a: Exp[A], b: Exp[B], c: Exp[C], d: Exp[D]) =>
    isolateIf(copy) {
      register(lambda4.inputA -> a)
      register(lambda4.inputB -> b)
      register(lambda4.inputC -> c)
      register(lambda4.inputD -> d)
      val block = blockToFunction0(lambda4, copy)
      block()
    }
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

  def isolateSubstScope[A](block: => A): A = isolateIf(true)(block)
  def isolateIf[A](cond: Boolean)(block: => A): A = {
    val save = subst
    val result = block
    if (cond) subst = save
    result
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
