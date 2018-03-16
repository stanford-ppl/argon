package argon.lang

import argon.core._
import argon.nodes._
import forge._

case class Array[T:Type](s: Exp[Array[T]]) extends MetaAny[Array[T]] {
  override type Internal = scala.Array[Any] // TODO: Way to fix this?

  /** Returns the size of this Array. **/
  @api def length: Index = wrap{ Array.length(this.s) }

  /** Returns the element at index `i`. **/
  @api def apply(i: Index): T = wrap{ Array.apply(this.s, i.s) }
  /** Updates the element at index `i` to data. **/
  @api def update[A](i: Index, data: A)(implicit lift: Lift[A,T]): MUnit = Unit(Array.update(this.s,i.s,lift(data).s))

  /** Applies the function **func** on each element in the Array. **/
  @api def foreach(func: T => MUnit): MUnit
    = Unit(Array.foreach(this.s, {t: Exp[T] => func(wrap(t)).s}, fresh[Index]))
  /** Returns a new Array created using the mapping `func` over each element in this Array. **/
  @api def map[R:Type](func: T => R): Array[R]
    = Array(Array.map(this.s, {t: Exp[T] => func(wrap(t)).s}, fresh[Index]))
  /** Returns a new Array created using the pairwise mapping `func` over each element in this Array
    * and the corresponding element in `that`.
    */
  @api def zip[S:Type,R:Type](that: Array[S])(func: (T,S) => R): Array[R]
    = Array(Array.zip(this.s, that.s, {(a:Exp[T],b:Exp[S]) => func(wrap(a), wrap(b)).s }, fresh[Index]))
  /** Reduces the elements in this Array into a single element using associative function `rfunc`. **/
  @api def reduce(rfunc: (T,T) => T): T
    = wrap{ Array.reduce(this.s,{(a:Exp[T],b:Exp[T]) => rfunc(wrap(a),wrap(b)).s}, fresh[Index], (fresh[T],fresh[T])) }

  /**
    * Reduces the elements in this Array and the given initial value into a single element
    * using associative function `rfunc`.
    */
  @api def fold(init: T)(rfunc: (T,T) => T): T
    = wrap{ Array.fold(this.s, init.s, {(a:Exp[T],b:Exp[T]) => rfunc(wrap(a),wrap(b)).s}, fresh[Index], (fresh[T],fresh[T])) }
  /** Returns a new Array with all elements in this Array which satisfy the given predicate `cond`. **/
  @api def filter(cond: T => MBoolean): Array[T]
    = Array(Array.filter(this.s, {t:Exp[T] => cond(wrap(t)).s}, fresh[Index]))
  /** Returns a new Array created by concatenating the results of `func` applied to all elements in this Array. **/
  @api def flatMap[R:Type](func: T => Array[R]): Array[R]
    = Array(Array.flatmap(this.s,{t:Exp[T] => func(wrap(t)).s}, fresh[Index]))

  /** Creates a string representation of this Array using the given `delimeter`. **/
  @api def mkString(delimeter: MString): MString = this.mkString("", delimeter, "")

  /** Creates a string representation of this Array using the given `delimeter`, bracketed by `start` and `stop`. **/
  @api def mkString(start: MString, delimeter: MString, stop: MString): MString
    = wrap(Array.string_mk(this.s, start.s, delimeter.s, stop.s))

  /** Returns true if this Array and `that` contain the same elements, false otherwise. **/
  @api def ===(that: Array[T]): MBoolean = this.zip(that){(x,y) => x === y }.reduce{_ && _}
  /** Returns true if this Array and `that` differ by at least one element, false otherwise. **/
  @api def =!=(that: Array[T]): MBoolean = this.zip(that){(x,y) => x =!= y }.reduce{_ || _}

  /** Concatenates two Arrays. **/
  @api def ++(that: Array[T]): Array[T]
    = Array.tabulate(this.length + that.length){i => ifThenElse(i < this.length, this.apply(i), that.apply(i))}

  @api def toText: MString = String.ify(this)
}

object Array {
  /** Static functions **/

  /** Returns an immutable Array with the given `size` and elements defined by `func`. **/
  @api def tabulate[T:Type](size: Index)(func: Index => T): MArray[T]
    = Array(mapindices(size.s, {i => func(wrap(i)).s}, fresh[Index]))
  /**
    * Returns an immutable Array with the given `size` and elements defined by `func`.
    * Note that while `func` does not depend on the index, it is still executed `size` times.
    */
  @api def fill[T:Type](size: Index)(func: => T): MArray[T] = this.tabulate(size){ _ => func}

  /** Returns an empty, mutable Array with the given `size`. **/
  @api def empty[T:Type](size: Index): MArray[T] = Array(mutable[T](size.s))

  /** Returns an immutable Array with the given elements. **/
  @api def apply[T:Type](elements: T*): MArray[T] = Array(from_seq(unwrap(elements)))


  /** Type **/
  implicit def arrayType[T:Type]: Type[Array[T]] = ArrayType(typ[T])

  /** Constructors **/
  @internal def length[T:Type](array: Exp[MArray[T]]): Sym[Index] = stage(ArrayLength(array))(ctx)
  @internal def string_mk[T:Type](x: Exp[MArray[T]], start: Exp[MString], delim: Exp[MString], end: Exp[MString]): Exp[MString] = stage(StringMk(x,start,delim,end))(ctx)

  @internal def apply[T:Type](array: Exp[MArray[T]], i: Exp[Index]): Sym[T] = stage(ArrayApply(array,i))(ctx)
  @internal def update[T:Type](array: Exp[MArray[T]], i: Exp[Index], e: Exp[T]): Sym[MUnit] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }

  @internal def mutable[T:Type](size: Exp[Index]): Sym[MArray[T]] = stageMutable(ArrayNew[T](size))(ctx)

  @internal def from_seq[T:Type](seq: Seq[Exp[T]]): Sym[MArray[T]] = stage(ArrayFromSeq(seq))(ctx)

  @internal def mapindices[T:Type](
    size: Exp[Index],
    func: Exp[Index] => Exp[T],
    i:    Bound[Index]
  ): Sym[MArray[T]] = {
    val blk = stageLambda1(i){ func(i) }
    val effects = blk.effects
    stageEffectful(MapIndices(size, blk, i), effects.star)(ctx)
  }

  @internal def foreach[T:Type](
    array: Exp[MArray[T]],
    func:  Exp[T] => Exp[MUnit],
    i:     Bound[Index]
  ): Sym[MUnit] = {
    val aBlk = stageLambda2(array, i){ apply(array,i) }
    val fBlk = stageLambda1(aBlk.result){ func(aBlk.result) }
    val effects = aBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayForeach(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  @internal def map[T:Type,R:Type](
    array: Exp[MArray[T]],
    func:  Exp[T] => Exp[R],
    i:     Bound[Index]
  ): Sym[Array[R]] = {
    val aBlk = stageLambda2(array, i) { apply(array,i) }
    val fBlk = stageLambda1(aBlk.result) { func(aBlk.result) }
    val effects = aBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayMap(array, aBlk, fBlk, i), effects.star)(ctx)
  }

  @internal def zip[A:Type,B:Type,C:Type](
    a:    Exp[MArray[A]],
    b:    Exp[MArray[B]],
    func: (Exp[A], Exp[B]) => Exp[C],
    i:    Bound[Index]
  ): Sym[MArray[C]] = {
    val aBlk = stageLambda2(a,i) { apply(a,i) }
    val bBlk = stageLambda2(b,i) { apply(b,i) }
    val fBlk = stageLambda2(aBlk.result, bBlk.result) { func(aBlk.result,bBlk.result) }
    val effects = aBlk.effects andAlso bBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayZip(a, b, aBlk, bBlk, fBlk, i), effects.star)(ctx)
  }

  @internal def reduce[A:Type](
    array:  Exp[MArray[A]],
    rfunc: (Exp[A], Exp[A]) => Exp[A],
    i:      Bound[Index],
    rV:     (Bound[A],Bound[A])
  ): Sym[A] = {
    val aBlk = stageLambda2(array,i) { apply(array,i) }
    val rBlk = stageLambda2(rV._1,rV._2) { rfunc(rV._1,rV._2) }
    val effects = aBlk.effects andAlso rBlk.effects
    stageEffectful(ArrayReduce(array,aBlk,rBlk,i,rV), effects.star)(ctx)
  }

  @internal def fold[A:Type](
    array: Exp[MArray[A]],
    init:  Exp[A],
    rfunc: (Exp[A], Exp[A]) => Exp[A],
    i:     Bound[Index],
    rV:    (Bound[A],Bound[A])
  ): Sym[A] = {
    val aBlk = stageLambda2(array,i){ apply(array,i) }
    val rBlk = stageLambda2(rV._1,rV._2){ rfunc(rV._1, rV._2) }
    val effects = aBlk.effects andAlso rBlk.effects
    stageEffectful(ArrayFold(array,init,aBlk,rBlk,i,rV), effects.star)(ctx)
  }

  @internal def filter[A:Type](
    array: Exp[MArray[A]],
    cond:  Exp[A] => Exp[MBoolean],
    i:     Bound[Index]
  ): Sym[MArray[A]] = {
    val aBlk = stageLambda2(array, i) { apply(array, i) }
    val cBlk = stageLambda1(aBlk.result) { cond(aBlk.result) }
    val effects = aBlk.effects andAlso cBlk.effects
    stageEffectful(ArrayFilter(array,aBlk,cBlk,i), effects.star)(ctx)
  }

  @internal def flatmap[A:Type,B:Type](
    array: Exp[MArray[A]],
    func:  Exp[A] => Exp[MArray[B]],
    i:     Bound[Index]
  ): Sym[MArray[B]] = {
    val aBlk = stageLambda2(array, i) { apply(array, i) }
    val fBlk = stageLambda1(aBlk.result) { func(aBlk.result) }
    val effects = aBlk.effects andAlso fBlk.effects
    stageEffectful(ArrayFlatMap(array,aBlk,fBlk,i), effects.star)(ctx)
  }

  @internal private[argon] def input_arguments(): MArray[MString] = Array(stage(InputArguments())(ctx))
}
