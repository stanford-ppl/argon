package argon.lang

import argon._
import argon.nodes._
import forge._
import org.virtualized.EmptyContext

case class Array[T:Type](s: Exp[Array[T]]) extends MetaAny[Array[T]] {
  private val mT = implicitly[Type[T]]
  override type Internal = scala.Array[mT.Internal]

  protected val array = Array
  @api def length: Index = wrap{ array.length(this.s) }

  @api def apply(i: Index): T = wrap{ array.apply(this.s, i.s) }
  @api def update[A](i: Index, data: A)(implicit lift: Lift[A,T]): MUnit
    = wrap{ array.update(this.s,i.s,lift(data).s) }

  @api def foreach(func: T => MUnit): MUnit
    = wrap{ array.foreach(this.s, {t => func(wrap(t)).s}, fresh[Index]) }
  @api def map[R:Type](func: T => R): Array[R]
    = Array(array.map(this.s, {t => func(wrap(t)).s}, fresh[Index]))
  @api def zip[S:Type,R:Type](that: Array[S])(func: (T,S) => R): Array[R]
    = Array(array.zip(this.s, that.s, {(a,b) => func(wrap(a), wrap(b)).s }, fresh[Index]))
  @api def reduce(rfunc: (T,T) => T): T
    = wrap{ array.reduce(this.s,{(a,b) => rfunc(wrap(a),wrap(b)).s}, fresh[Index], (fresh[T],fresh[T])) }
  @api def filter(cond: T => MBoolean): Array[T]
    = Array(array.filter(this.s, {t => cond(wrap(t)).s}, fresh[Index]))
  @api def flatMap[R:Type](func: T => Array[R]): Array[R]
    = Array(array.flatmap(this.s,{t => func(wrap(t)).s}, fresh[Index]))

  @api def ===(that: Array[T]): MBoolean = this.zip(that){(x,y) => x === y }.reduce{_ && _}
  @api def =!=(that: Array[T]): MBoolean = this.zip(that){(x,y) => x =!= y }.reduce{_ || _}
  @api def toText: MString = String.ify(this)
}

object Array {
  @api def tabulate[T:Type](size: Index)(func: Index => T): Array[T]
    = Array(mapindices(size.s, {i => func(wrap(i)).s}, fresh[Index]))
  @api def fill[T:Type](size: Index)(func: => T): Array[T] = this.tabulate(size){ _ => func}
  @api def empty[T:Type](size: Index): Array[T] = Array(mutable[T](size.s))


  /** Constructors **/
  @internal def length(array: Exp[Array[_]]): Sym[Index] = stage(ArrayLength(array))(ctx)

  @internal def apply[T:Type](array: Exp[Array[T]], i: Exp[Index]): Sym[T] = stage(ArrayApply(array,i))(ctx)
  @internal def update[T:Type](array: Exp[Array[T]], i: Exp[Index], e: Exp[T]): Sym[MUnit] = {
    stageWrite(array)(ArrayUpdate(array,i,e))(ctx)
  }

  @internal def mutable[T:Type](size: Exp[Index]): Sym[Array[T]] = stageMutable(ArrayNew[T](size))(ctx)

  @internal def mapindices[T:Type](
    size: Exp[Index],
    func: Exp[Index] => Exp[T],
    i:    Bound[Index]
  ): Sym[Array[T]] = {
    val blk = stageLambda(i){ func(i) }
    val effects = blk.effects
    stageEffectful(MapIndices(size, blk, i), effects.star)(ctx)
  }

  @internal def foreach[T:Type](
    array: Exp[Array[T]],
    func:  Exp[T] => Exp[MUnit],
    i:     Bound[Index]
  ): Sym[MUnit] = {
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
    cond:  Exp[A] => Exp[MBoolean],
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

  @internal private[argon] def input_arguments(ctx: SrcCtx, state: State): Array[MString] = wrap(stage(InputArguments())(ctx))
}


trait ArrayExp {
  /** Type **/
  implicit def arrayType[T:Type]: Type[Array[T]] = ArrayType(typ[T])
}

