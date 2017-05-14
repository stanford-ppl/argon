package argon.exp

import org.virtualized.EmptyContext
import argon._
import forge._

case class Array[T:Type](s: Exp[Array[T]]) extends MetaAny[Array[T]] {
  @api def length: Index = wrap{ ArrayExp.length(this.s) }

  @api def apply(i: Index): T = wrap{ ArrayExp.apply(this.s, i.s) }
  @api def update[A](i: Index, data: A)(implicit lift: Lift[A,T]): Void = wrap{ ArrayExp.update(this.s,i.s,data.s) }

  @api def foreach(func: T => Void): Void = wrap{ ArrayExp.foreach(this.s, {t => func(wrap(t)).s}, fresh[Index]) }
  @api def map[R:Type](func: T => R): Array[R] = wrap{ ArrayExp.map(this.s, {t => func(wrap(t)).s}, fresh[Index]) }
  @api def zip[S:Type,R:Type](that: Array[S])(func: (T,S) => R): Array[R] = wrap {
    ArrayExp.zip(this.s, that.s, {(a,b) => func(wrap(a), wrap(b)).s }, fresh[Index])
  }
  @api def reduce(rfunc: (T,T) => T): T = wrap{
    ArrayExp.reduce(this.s,{(a,b) => rfunc(wrap(a),wrap(b)).s}, fresh[Index], (fresh[T],fresh[T]))
  }
  @api def filter(cond: T => Bool): Array[T] = wrap{ ArrayExp.filter(this.s, {t => cond(wrap(t)).s}, fresh[Index]) }

  @api def flatMap[R:Type](func: T => Array[R]): Array[R] = wrap{
    ArrayExp.flatmap(this.s,{t => func(wrap(t)).s}, fresh[Index])
  }

  @api def ===(that: Array[T]): Bool = this.zip(that){(x,y) => x === y }.reduce{_ && _}
  @api def =!=(that: Array[T]): Bool = this.zip(that){(x,y) => x =!= y }.reduce{_ || _}
  @api def toText: Text = textify(this)
}

object Array {
  @api def tabulate[T:Type](size: Index)(func: Index => T): Array[T] = wrap{
    ArrayExp.mapindices(size.s, {i => func(wrap(i)).s}, fresh[Index])
  }
  @api def fill[T:Type](size: Index)(func: => T): Array[T] = this.tabulate(size){ _ => func}
  @api def empty[T:Type](size: Index): Array[T] = wrap{ ArrayExp.mutable[T](size.s) }
}




/** IR Nodes **/
case class InputArguments() extends Op[Array[Text]] {
  def mirror(f:Tx) = stage(InputArguments())(EmptyContext)
}

case class ArrayNew[T:Type](size: Exp[Index]) extends Op2[T,Array[T]] {
  def mirror(f:Tx) = ArrayExp.mutable[T](f(size))
}

case class ArrayApply[T:Type](array: Exp[Array[T]], i: Exp[Index]) extends Op[T] {
  def mirror(f:Tx) = ArrayExp.apply(f(array),f(i))
  override def aliases = Nil
  //override def extracts = dyns(array) TODO: Why does this cause issues?
}

case class ArrayUpdate[T:Type](array: Exp[Array[T]], i: Exp[Index], e: Exp[T]) extends Op[Void] {
  def mirror(f:Tx) = ArrayExp.update(f(array),f(i),f(e))
  override def contains = dyns(e)
}

case class ArrayLength[T:Type](array: Exp[Array[T]]) extends Op[Index] {
  def mirror(f:Tx) = ArrayExp.length(f(array))
}

case class MapIndices[T:Type](
  size: Exp[Index],
  func: Lambda1[Index,T],
  i:    Bound[Index]
) extends Op[Array[T]] {
  def mirror(f:Tx) = ArrayExp.mapindices(f(size),f(func),i)
  override def inputs = dyns(size) ++ dyns(func)
  override def freqs  = normal(size) ++ hot(func)
  override def binds  = i +: super.binds

  override def aliases = Nil

  val mT = typ[T]
}

case class ArrayForeach[T:Type](
  array: Exp[Array[T]],
  apply: Lambda2[Array[T],Index,T],
  func:  Lambda1[T,Void],
  i:     Bound[Index]
) extends Op[Void] {
  def mirror(f:Tx) = ArrayExp.foreach(f(array),f(apply),f(func),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
  override def binds = i +: super.binds
  val mT = typ[T]
}

case class ArrayMap[T:Type,S:Type](
  array: Exp[Array[T]],
  apply: Lambda2[Array[T],Index,T],
  func:  Lambda1[T,S],
  i:     Bound[Index]
) extends Op[Array[S]] {
  def mirror(f:Tx) = ArrayExp.map(f(array),f(apply),f(func),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
  override def binds = i +: super.binds

  override def aliases = Nil

  val mT = typ[T]
  val mS = typ[S]
}

case class ArrayZip[A:Type,B:Type,C:Type](
  arrayA: Exp[Array[A]],
  arrayB: Exp[Array[B]],
  applyA: Lambda2[Array[A],Index,A],
  applyB: Lambda2[Array[B],Index,B],
  func:   Lambda2[A,B,C],
  i:      Bound[Index]
) extends Op[Array[C]] {
  def mirror(f:Tx) = ArrayExp.zip(f(arrayA),f(arrayB),f(applyA),f(applyB),f(func),i)
  override def inputs = dyns(arrayA) ++ dyns(arrayB) ++ dyns(applyA) ++ dyns(applyB) ++ dyns(func)
  override def freqs  = normal(arrayA) ++ normal(arrayB) ++ hot(applyA) ++ hot(applyB) ++ hot(func)
  override def binds = i +: super.binds

  override def aliases = Nil

  val mA = typ[A]
  val mB = typ[B]
  val mC = typ[C]
}

case class ArrayReduce[A:Type](
  array:  Exp[Array[A]],
  apply:  Lambda2[Array[A],Index,A],
  reduce: Lambda2[A,A,A],
  i:      Bound[Index],
  rV:     (Bound[A],Bound[A])
) extends Op[A] {
  def mirror(f:Tx) = ArrayExp.reduce(f(array),f(apply),f(reduce),i,rV)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(reduce)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(reduce)
  override def binds = super.binds ++ Seq(i, rV._1, rV._2)

  val mA = typ[A]
}

case class ArrayFilter[A:Type](
  array: Exp[Array[A]],
  apply: Lambda2[Array[A],Index,A],
  cond:  Lambda1[A,Bool],
  i:     Bound[Index]
) extends Op[Array[A]] {
  def mirror(f:Tx) = ArrayExp.filter(f(array),f(apply),f(cond),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(cond)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(cond)
  override def binds = i +: super.binds

  override def aliases = Nil
}

case class ArrayFlatMap[A:Type,B:Type](
  array: Exp[Array[A]],
  apply: Lambda2[Array[A],Index,A],
  func:  Lambda1[A,Array[B]],
  i:     Bound[Index]
) extends Op[Array[B]] {
  def mirror(f:Tx) = ArrayExp.flatmap(f(array),f(apply),f(func),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
  override def binds = i +: super.binds

  override def aliases = Nil
}

object ArrayExp {
  /** Constructors **/
  @internal def length[T:Type](array: Exp[Array[T]]): Sym[Index] = stage(ArrayLength(array))(ctx)

  @internal def apply[T:Type](array: Exp[Array[T]], i: Exp[Index]): Sym[T] = stage(ArrayApply(array,i))(ctx)
  @internal def update[T:Type](array: Exp[Array[T]], i: Exp[Index], e: Exp[T]): Sym[Void] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }

  @internal def mutable[T:Type](size: Exp[Index]): Sym[Array[T]] = stageMutable(ArrayNew[T](size))(ctx)

  @internal def mapindices[T:Type](
    size: Exp[Index],
    func: Exp[Index] => Exp[T],
    i:    Bound[Index]
  ): Sym[Array[T]] = {
    val blk = stageBlock{ func }
    val effects = blk.effects
    stageEffectful(MapIndices(size, blk, i), effects.star)(ctx)
  }

  @internal def foreach[T:Type](
    array: Exp[Array[T]],
    func:  Exp[T] => Exp[Void],
    i:     Bound[Index]
  ): Sym[Void] = {
    val aBlk = stageLambda(array, i){ apply(array,i) }
    val fBlk = stageLambda(aBlk.result){ func(aBlk.result) }
    val effects = aBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayForeach(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  @internal def map[T:Type,R:Type](
    array: Exp[Array[T]],
    func:  Exp[T] => Exp[R],
    i:     Bound[Index]
  ): Sym[Array[R]] = {
    val aBlk = stageLambda(array, i) { apply(array,i) }
    val fBlk = stageLambda(aBlk.result) { func(aBlk.result) }
    val effects = aBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayMap(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  @internal def zip[A:Type,B:Type,C:Type](
    a:    Exp[Array[A]],
    b:    Exp[Array[B]],
    func: (Exp[A], Exp[B]) => Exp[C],
    i:    Bound[Index]
  ): Sym[Array[C]] = {
    val aBlk = stageLambda(a,i) { apply(a,i) }
    val bBlk = stageLambda(b,i) { apply(a,i) }
    val fBlk = stageLambda(aBlk.result, bBlk.result) { func(aBlk.result,bBlk.result) }
    val effects = aBlk.effects andAlso bBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayZip(a, b, aBlk, bBlk, fBlk, i), effects.star)(ctx)
  }

  @internal def reduce[A:Type](
    array:  Exp[Array[A]],
    rfunc: (Exp[A], Exp[A]) => Exp[A],
    i:      Bound[Index],
    rV:     (Bound[A],Bound[A])
  ): Sym[A] = {
    val aBlk = stageLambda(array,i) { apply(array,i) }
    val rBlk = stageLambda(rV._1,rV._2) { rfunc(rV._1,rV._2) }
    val effects = aBlk.effects andAlso rBlk.effects
    stageEffectful(ArrayReduce(array,aBlk,rBlk,i,rV), effects.star)(ctx)
  }

  @internal def filter[A:Type](
    array: Exp[Array[A]],
    cond:  Exp[A] => Exp[Bool],
    i:     Bound[Index]
  ): Sym[Array[A]] = {
    val aBlk = stageLambda(array, i) { apply(array, i) }
    val cBlk = stageLambda(aBlk.result) { cond(aBlk.result) }
    val effects = aBlk.effects andAlso cBlk.effects
    stageEffectful(ArrayFilter(array,aBlk,cBlk,i), effects.star)(ctx)
  }

  @internal def flatmap[A:Type,B:Type](
    array: Exp[Array[A]],
    func:  Exp[A] => Exp[Array[B]],
    i:     Bound[Index]
  ): Sym[Array[B]] = {
    val aBlk = stageLambda(array, i) { apply(array, i) }
    val fBlk = stageLambda(aBlk.result) { func(aBlk.result) }
    val effects = aBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayFlatMap(array,aBlk,fBlk,i), effects.star)(ctx)
  }
}
