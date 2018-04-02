package argon.nodes

import argon.core._
import argon.compiler._
import virtualized.EmptyContext

case class ArrayType[T](child: Type[T]) extends Type[MArray[T]] {
  override def wrapped(s: Exp[MArray[T]]): MArray[T] = new MArray(s)(child)
  override def typeArguments = List(child)
  override def stagedClass = classOf[MArray[T]]
  override def isPrimitive = false
}

/** IR Nodes **/
case class InputArguments() extends Op[MArray[MString]] {
  def mirror(f:Tx) = stage(InputArguments())(EmptyContext)
}

case class ArrayNew[T:Type](size: Exp[Index]) extends Op2[T,MArray[T]] {
  def mirror(f:Tx) = MArray.mutable[T](f(size))
}

case class ArrayFromSeq[T:Type](seq: Seq[Exp[T]]) extends Op2[T,MArray[T]] {
  def mirror(f:Tx) = MArray.from_seq(f(seq))
}

case class ArrayApply[T:Type](coll: Exp[MArray[T]], i: Exp[Index]) extends Op[T] with AtomicRead[MArray[T]] {
  def mirror(f:Tx) = MArray.apply(f(coll),f(i))
  override def aliases = Nil
  //override def extracts = dyns(array) TODO: Why does this cause issues?
}

case class ArrayUpdate[T:Type](array: Exp[MArray[T]], i: Exp[Index], e: Exp[T]) extends Op[MUnit] {
  def mirror(f:Tx) = MArray.update(f(array),f(i),f(e))
  override def contains = dyns(e)
}

case class ArrayLength[T:Type](array: Exp[MArray[T]]) extends Op[Index] {
  def mirror(f:Tx) = MArray.length(f(array))
}

case class MapIndices[T:Type](
  size: Exp[Index],
  func: Lambda1[Index,T],
  i:    Bound[Index]
) extends Op[MArray[T]] {
  def mirror(f:Tx) = MArray.mapindices(f(size),f(func),i)
  override def inputs = dyns(size) ++ dyns(func)
  override def freqs  = normal(size) ++ hot(func)
  override def binds  = i +: super.binds

  override def aliases = Nil

  val mT = typ[T]
}

case class ArrayForeach[T:Type](
  array: Exp[MArray[T]],
  apply: Lambda2[MArray[T],Index,T],
  func:  Lambda1[T,MUnit],
  i:     Bound[Index]
) extends Op[MUnit] {
  def mirror(f:Tx) = MArray.foreach(f(array),f(func),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
  override def binds = i +: super.binds
  val mT = typ[T]
}

case class ArrayMap[T:Type,S:Type](
  array: Exp[MArray[T]],
  apply: Lambda2[MArray[T],Index,T],
  func:  Lambda1[T,S],
  i:     Bound[Index]
) extends Op[MArray[S]] {
  def mirror(f:Tx) = MArray.map(f(array),f(func),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
  override def binds = i +: super.binds

  override def aliases = Nil

  val mT = typ[T]
  val mS = typ[S]
}

case class ArrayZip[A:Type,B:Type,C:Type](
  arrayA: Exp[MArray[A]],
  arrayB: Exp[MArray[B]],
  applyA: Lambda2[MArray[A],Index,A],
  applyB: Lambda2[MArray[B],Index,B],
  func:   Lambda2[A,B,C],
  i:      Bound[Index]
) extends Op[MArray[C]] {
  def mirror(f:Tx) = MArray.zip(f(arrayA),f(arrayB),f(func),i)
  override def inputs = dyns(arrayA) ++ dyns(arrayB) ++ dyns(applyA) ++ dyns(applyB) ++ dyns(func)
  override def freqs  = normal(arrayA) ++ normal(arrayB) ++ hot(applyA) ++ hot(applyB) ++ hot(func)
  override def binds = i +: super.binds

  override def aliases = Nil

  val mA = typ[A]
  val mB = typ[B]
  val mC = typ[C]
}

case class ArrayReduce[A:Type](
  array:  Exp[MArray[A]],
  apply:  Lambda2[MArray[A],Index,A],
  reduce: Lambda2[A,A,A],
  i:      Bound[Index],
  rV:     (Bound[A],Bound[A])
) extends Op[A] {
  def mirror(f:Tx) = MArray.reduce(f(array),f(reduce),i,rV)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(reduce)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(reduce)
  override def binds = super.binds ++ Seq(i, rV._1, rV._2)
  val mA = typ[A]
}

case class ArrayFold[A:Type](
  array:  Exp[MArray[A]],
  init:   Exp[A],
  apply:  Lambda2[MArray[A],Index,A],
  reduce: Lambda2[A,A,A],
  i:      Bound[Index],
  rV:     (Bound[A],Bound[A])
) extends Op[A] {
  def mirror(f:Tx) = MArray.fold(f(array),f(init),f(reduce),i,rV)
  override def inputs = dyns(array) ++ dyns(init) ++ dyns(apply) ++ dyns(reduce)
  override def freqs  = normal(array) ++ normal(init) ++ hot(apply) ++ hot(reduce)
  override def binds  = super.binds ++ Seq(i, rV._1, rV._2)
  val mA = typ[A]
}

case class ArrayFilter[A:Type](
  array: Exp[MArray[A]],
  apply: Lambda2[MArray[A],Index,A],
  cond:  Lambda1[A,MBoolean],
  i:     Bound[Index]
) extends Op[MArray[A]] {
  def mirror(f:Tx) = MArray.filter(f(array),f(cond),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(cond)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(cond)
  override def binds = i +: super.binds

  override def aliases = Nil
}

case class ArrayFlatMap[A:Type,B:Type](
  array: Exp[MArray[A]],
  apply: Lambda2[MArray[A],Index,A],
  func:  Lambda1[A,MArray[B]],
  i:     Bound[Index]
) extends Op[MArray[B]] {
  def mirror(f:Tx) = MArray.flatmap(f(array),f(func),i)
  override def inputs = dyns(array) ++ dyns(apply) ++ dyns(func)
  override def freqs  = normal(array) ++ hot(apply) ++ hot(func)
  override def binds = i +: super.binds

  override def aliases = Nil
}
