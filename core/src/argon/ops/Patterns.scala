package argon.ops

import argon.core.Staging
/*
trait PatternApi extends PatternExp
trait PatternExp extends Staging with FltPtExp with FixPtExp with VoidExp with TextExp /*{

  abstract class ParallelCollection[A:StageAny]


  /** Parallel Elems **/
  trait LoopElem { val chunks: Int }

  /** Foreach elem **/
  case class ForeachElem[A:StageAny](
    func:   Block[A],   // Foreach function
    chunks: Int
  ) extends Op[Void] with LoopElem {
    def mirror(f:Tx) = foreach_elem(f(func),chunks)
    val mA = typ[A]
  }

  /** Collect elems **/
  sealed abstract class CollectType
  /** Statically sized map plus effect dependencies not captured by the elem **/
  case class CollectMap(elem: Block[_], effects: Seq[Exp[_]]) extends CollectType
  /** Dynamically sized map (e.g. FileReader) plus effect dependencies not captured by the elem **/
  case class CollectDynamicMap(elem: Block[_], effects: Seq[Exp[_]]) extends CollectType
  /** Filter - includes always effects, the condition symbol, then effects and else effects **/
  case class CollectFilter(
    otherEffects: Seq[Exp[_]],  // Effects that are always done (i.e. before the condition)
    cond:         Exp[Bool],    // Result symbol of the condition
    thenBlock:    Block[_],     // Block for when the condition is true
    thenEffects:  Seq[Exp[_]],  // Effects executed only when `cond` is true
    elseEffects:  Seq[Exp[_]]   // Effects executed only when `cond` is false
  ) extends CollectType
  /** FlatMap **/
  case class CollectFlatMap() extends CollectType

  // Matches on static and dynamically sized Maps
  object CollectAnyMap {
    def unapply(collect: CollectType): Option[(Block[_],Seq[Exp[_]])] = collect match {
      case CollectMap(elem, effects) => Some((elem, effects))
      case CollectDynamicMap(elem, effects) => Some((elem,effects))
      case _ => None
    }
  }
  // Determines whether the Elem represents a special collect case
  def getCollectType(e: CollectBaseElem[_,_]): CollectType


  /** Output collection/strategy of a CollectElem loop body **/
  abstract class CollectOutput[A:StageAny, I:Staged, CA:StageAny] {

  }

  abstract class CollectBaseElem[A:StageAny, R:StageAny](
    val iFunc:       Block[ParallelCollection[A]],  // FlatMap function - produces intermediate collection each iteration
    val unknownSize: Boolean,                       // Dynamic output size - true for FlatMap, generally false otherwise
    val chunks:      Int,
    val j:           Bound[Index],                  // Iterator for inner loop (appending intermediate to final result)
    val sizeInner:   Block[Index],                  // Size of the intermediate collection (range for inner loop)
    val applyInner:  Block[A]                       // Element of intermediate collection at j
  ) extends Op2[A,R] with LoopElem

  case class CollectElem[A:StageAny, I<:ParallelCollection[A]:Staged, CA<:ParallelCollection[A]:Staged](
    buffer:                   CollectOutput[A,I,CA],
    override val iFunc:       Block[ParallelCollection[A]],
    override val unknownSize: Boolean,
    override val chunks:      Int,
    override val j:           Bound[Index],
    override val sizeInner:   Block[Index],
    override val applyInner:  Block[A]
  ) extends CollectBaseElem[A,CA](iFunc,unknownSize,chunks,j,sizeInner,applyInner) {
    def mirror(f:Tx) = collect_elem(buffer,f(iFunc),unknownSize,chunks,j,f(sizeInner),f(applyInner))
  }



  /** Constructors **/
  def foreach_elem[A:StageAny](func: => Exp[A], chunks: Int)(implicit ctx: SrcCtx): Sym[Void] = {
    val blk = stageBlock{ func }
    val effects = blk.summary
    stageEffectful(ForeachElem(blk, chunks), effects.star)(ctx)
  }

  def collect_elem[A:StageAny, I<:ParallelCollection[A]:Staged, CA<:ParallelCollection[A]:Staged](
    buffer:      CollectOutput[A,I,CA],
    iFunc:       => Exp[ParallelCollection[A]],
    unknownSize: Boolean,
    chunks:      Int,
    j:           Bound[Index],
    sizeInner:   => Exp[Index],
    applyInner:  => Exp[A]
  )(implicit ctx: SrcCtx): Sym[CA] = {
    val iBlk     = stageBlock { iFunc }
    val sizeBlk  = stageLambda(iBlk.result){ sizeInner }
    val applyBlk = stageLambda(iBlk.result){ applyInner }
    val effects = iBlk.summary andAlso (sizeBlk.summary andAlso applyBlk.summary).star
    stageEffectful(CollectElem[A,I,CA](buffer,iBlk,unknownSize,chunks,j,sizeBlk,applyBlk), effects)(ctx)
  }
}*/
*/
