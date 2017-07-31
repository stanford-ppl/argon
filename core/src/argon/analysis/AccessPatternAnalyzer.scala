package argon.analysis

import argon.core._
import argon.compiler._
import argon.UndefinedAccessPatternException
import argon.transform.Transformer
import argon.traversal.Traversal
import forge._

sealed abstract class AffineFunction {
  type Tx = argon.transform.Transformer
  def mirror(f:Tx): AffineFunction
  def eval(f: Exp[Index] => Int): Int
}
class Prod(val x: Seq[Either[Exp[Index],AffineFunction]]) extends AffineFunction {
  def mirror(f:Tx) = new Prod(x.map{
    case Left(e) => Left(f(e))
    case Right(af) => Right(af.mirror(f))
  })
  def eval(f: Exp[Index] => Int): Int = x.map{
    case Left(e) => f(e)
    case Right(af) => af.eval(f)
  }.product

  override def toString: String = x.map{case Left(e) => c"$e"; case Right(af) => af.toString}.mkString(" * ")
}
object Prod {
  def apply(x: Exp[Index]*) = new Prod(x.map(y => Left(y)))
}
class Sum(val x: Seq[Either[Exp[Index],AffineFunction]]) extends AffineFunction {
  def mirror(f:Tx) = new Sum(x.map{
    case Left(e) => Left(f(e))
    case Right(af) => Right(af.mirror(f))
  })
  def eval(f: Exp[Index] => Int): Int = x.map{
    case Left(e) => f(e)
    case Right(af) => af.eval(f)
  }.sum

  override def toString: String = x.map{case Left(e) => c"$e"; case Right(af) => af.toString}.mkString(" + ")
}
object Sum {
  def apply(x: Exp[Index]) = new Sum(Seq(Left(x)))
  def apply(x: Seq[Either[Exp[Index],AffineFunction]]) = new Sum(x)
}
case object One extends Prod(Nil) { override def toString: String = "One" }
case object Zero extends Sum(Nil) { override def toString: String = "Zero" }

// Variations used here allow Index to be abstract (otherwise can't as easily define stride of 1)
sealed abstract class IndexPattern {
  def index: Option[Exp[Index]]
  def isGeneral: Boolean = this.isInstanceOf[GeneralAffine] || this.isInstanceOf[GeneralOffset]
}

// product(a)*i + sum(b), where all elements in a and b must be loop invariant relative to i
case class GeneralAffine(a: AffineFunction, i: Exp[Index]) extends IndexPattern {
  def index = Some(i)
}
case class GeneralOffset(b: AffineFunction) extends IndexPattern { def index = None }

// a*i + b, where a and b must be loop invariant
case class AffineAccess(a: Exp[Index], i: Exp[Index], b: Exp[Index]) extends IndexPattern { def index = Some(i) }
// i + b, where b must be loop invariant
case class OffsetAccess(i: Exp[Index], b: Exp[Index]) extends IndexPattern { def index = Some(i) }
// a*i, where a must be loop invariant
case class StridedAccess(a: Exp[Index], i: Exp[Index]) extends IndexPattern { def index = Some(i) }
// linear access with some loop iterator
case class LinearAccess(i: Exp[Index]) extends IndexPattern { def index = Some(i) }
// loop invariant access (but may change with outer loops)
case class InvariantAccess(b: Exp[Index]) extends IndexPattern { def index = None }
// anything else
case object RandomAccess extends IndexPattern { def index = None }

case class AccessPattern(indices: Seq[IndexPattern]) extends Metadata[AccessPattern] {
  def mirror(f:Tx) = AccessPattern(indices.map{
    case GeneralAffine(a,i)  => GeneralAffine(a.mirror(f),f(i))
    case GeneralOffset(b)    => GeneralOffset(b.mirror(f))

    case AffineAccess(a,i,b) => AffineAccess(f(a),f(i),f(b))
    case OffsetAccess(i,b)   => OffsetAccess(f(i), f(b))
    case StridedAccess(a,i)  => StridedAccess(f(a),f(i))
    case LinearAccess(i)     => LinearAccess(f(i))
    case InvariantAccess(b)  => InvariantAccess(f(b))
    case RandomAccess        => RandomAccess
  })
}

@data object accessPatternOf {
  def apply(x: Exp[_]): Seq[IndexPattern] = accessPatternOf.get(x).getOrElse{ throw new UndefinedAccessPatternException(x) }
  def update(x: Exp[_], indices: Seq[IndexPattern]) { metadata.add(x, AccessPattern(indices)) }
  def get(x: Exp[_]): Option[Seq[IndexPattern]] = metadata[AccessPattern](x).map(_.indices)
}

trait AccessPatternAnalyzer extends Traversal {

  // All loop indices encountered above the current scope
  var loopIndices = Set[Bound[Index]]()
  var loopFromIndex = Map[Bound[Index], Exp[_]]()
  var boundIndexPatterns = Map[Exp[Index], Seq[IndexPattern]]()

  // The list of statements which can be scheduled prior to block traversals
  // (All statements in all scopes below this scope)
  // If a statement is not contained in this set, it must be loop invariant (because it was already scheduled)
  var innerScopes = Map[Exp[Index],Seq[Stm]]()

  // The exact scope of the current loop
  var loopScope: Seq[Stm] = Nil

  private def inLoop[T](loop: Exp[_], indices: Seq[Bound[Index]])(blk: => T): T = {
    val inner = innerStms
    indices.foreach{i => innerScopes += i -> inner}

    val prevIndices = loopIndices
    val prevLoops = loopFromIndex
    loopIndices ++= indices
    loopFromIndex ++= indices.map{i => i -> loop}
    val result = blk
    loopIndices = prevIndices
    loopFromIndex = prevLoops
    result
  }
  override protected def visitBlock[S](block: Block[S]) = {
    tab += 1
    traverseStmsInBlock(block, {stms =>
      val prevLoopScope = loopScope
      loopScope = stms
      visitStms(stms)
      loopScope = prevLoopScope
    })
    tab -= 1
    block
  }

  /** Abstract functions. To use, fill in w/ relevant node matching.
    * These are defined on Exp rather than Def to allow accessing metadata
    * without mucking with the (symbol,def) table.
    * Use Def or Op unapply methods to get corresponding Def of symbols
    **/
  // Pair of symbols for nodes used in address calculation addition nodes
  def indexPlusUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])]
  // Pair of symbols for nodes used in address calculation multiplication nodes
  def indexTimesUnapply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])]
  // List of loop scopes. Each scope contains a list of iterators and scopes to traverse for loop nodes
  def loopUnapply(x: Exp[_]): Option[Seq[(Seq[Bound[Index]], Seq[Block[_]])]]
  // Memory being read + list of addresses (for N-D access)
  def readUnapply(x: Exp[_]): Option[(Exp[_], Seq[Exp[Index]])]
  // Memory being written + list of addresses (for N-D access)
  def writeUnapply(x: Exp[_]): Option[(Exp[_], Seq[Exp[Index]])]

  def indexUnapply(x: Exp[Index]): Option[Bound[Index]] = x match {
    case s: Bound[_] if loopIndices.contains(s.asInstanceOf[Bound[Index]]) => Some(s.asInstanceOf[Bound[Index]])
    case _ => None
  }

  object Plus       { def unapply(x: Exp[Index]) = indexPlusUnapply(x) }
  object Times      { def unapply(x: Exp[Index]) = indexTimesUnapply(x) }
  object LoopLevels { def unapply(x: Exp[_]) = loopUnapply(x) }
  object MemRead    { def unapply(x: Exp[_]) = readUnapply(x) }
  object MemWrite   { def unapply(x: Exp[_]) = writeUnapply(x) }
  object LoopIndex  { def unapply(x: Exp[Index]) = indexUnapply(x) }

  /**
    * Check if expression b is invariant with respect to loop index i
    * An expression b is invariant to loop index i if it is defined outside of the loop scope of i
    */
  def isInvariant(b: Exp[Index], i: Bound[Index]): Boolean = b match {
    case Const(_) => true
    case Param(_) => true
    case s: Sym[_] => !innerScopes(i).exists{stm => stm.lhs.contains(s)}
    case _ => false
  }
  def isInvariantForAll(b: Exp[Index]): Boolean = loopIndices.forall{i => isInvariant(b,i) }

  def findGeneralAffinePattern(x: Exp[Index]): Seq[IndexPattern] = {
    dbg(c"Looking for affine access patterns from ${str(x)}")

    def extractPattern(x: Exp[Index]): Seq[IndexPattern] = x match {
      case Plus(a,b) => extractPattern(a) ++ extractPattern(b)
      case Times(LoopIndex(i), a) if isInvariant(a,i) => Seq(GeneralAffine(Prod(a),i))   // i*a
      case Times(a, LoopIndex(i)) if isInvariant(a,i) => Seq(GeneralAffine(Prod(a),i))   // a*i
      case LoopIndex(i) => Seq(GeneralAffine(One,i))                                     // i
      case b if isInvariantForAll(b) => Seq(GeneralOffset(Sum(b)))                       // b
      case _ => Seq(RandomAccess)
    }

    val pattern = extractPattern(x)

    dbg(c"Extracted pattern: " + pattern.mkString(" + "))

    if (pattern.contains(RandomAccess)) Seq(RandomAccess)
    else {
      val affine  = pattern.collect{case p: GeneralAffine => p }
      val groupedAffine = affine.groupBy(_.i)
                                .mapValues{funcs => funcs.map(af => Right(af.a) )}
                                .toList.map{case (i, as) => GeneralAffine(Sum(as),i) }

      val offsets = pattern.collect{case GeneralOffset(b) => Right(b) }
      val offset  = if (offsets.length == 1) GeneralOffset(offsets.head.value) else GeneralOffset(Sum(offsets))
      offset +: groupedAffine
    }
  }



  def extractIndexPattern(x: Exp[Index]): Seq[IndexPattern] = x match {
    case Plus(Times(LoopIndex(i), a), b) if isInvariant(a,i) && isInvariant(b,i) => Seq(AffineAccess(a,i,b)) // i*a + b
    case Plus(Times(a, LoopIndex(i)), b) if isInvariant(a,i) && isInvariant(b,i) => Seq(AffineAccess(a,i,b)) // a*i + b
    case Plus(b, Times(LoopIndex(i), a)) if isInvariant(a,i) && isInvariant(b,i) => Seq(AffineAccess(a,i,b)) // b + i*a
    case Plus(b, Times(a, LoopIndex(i))) if isInvariant(a,i) && isInvariant(b,i) => Seq(AffineAccess(a,i,b)) // b + a*i
    case Plus(LoopIndex(i), b) if isInvariant(b,i)  => Seq(OffsetAccess(i,b))                                // i + b
    case Plus(b, LoopIndex(i)) if isInvariant(b,i)  => Seq(OffsetAccess(i,b))                                // b + i
    case Times(LoopIndex(i), a) if isInvariant(a,i) => Seq(StridedAccess(a,i))                               // i*a
    case Times(a, LoopIndex(i)) if isInvariant(a,i) => Seq(StridedAccess(a,i))                               // a*i
    case LoopIndex(i) => Seq(LinearAccess(i))                                                                // i
    case b if isInvariantForAll(b) => Seq(InvariantAccess(b))                                                // b

    case _ => findGeneralAffinePattern(x)                                                                    // other
  }
  def extractAccessPatterns(xs: Seq[Exp[Index]]): Seq[IndexPattern] = xs.flatMap{
    case x if boundIndexPatterns.contains(x) => boundIndexPatterns(x)
    case x => extractIndexPattern(x)
  }

  override protected def visit(lhs: Sym[_], rhs: Op[_]): Unit = lhs match {
    case LoopLevels(levels) =>
      dbgs(s"[Loop] $lhs = $rhs")
      dbgs(s"  Levels: ${levels.length} ")
      dbgs(s"  Current indices: $loopIndices")
      levels.foreach { case (indices, blocks) =>
        dbgs(s"  Traversing loop level with $indices")
        inLoop(lhs, indices) {
          blocks.foreach { blk => visitBlock(blk) }
        }
      }

    case MemRead(mem, addresses) =>
      accessPatternOf(lhs) = extractAccessPatterns(addresses)
      dbgs(s"[Read] $lhs = $rhs")
      dbgs(s"  Memory: $mem")
      dbgs(s"  ND Address: $addresses")
      dbgs(s"  Current indices: $loopIndices")
      dbgs(s"  Access pattern: ${accessPatternOf(lhs)}")

    case MemWrite(mem, addresses) =>
      accessPatternOf(lhs) = extractAccessPatterns(addresses)
      dbgs(s"[Write] $lhs = $rhs")
      dbgs(s"  Memory: $mem")
      dbgs(s"  ND Address: $addresses")
      dbgs(s"  Current indices: $loopIndices")
      dbgs(s"  Access pattern: ${accessPatternOf(lhs)}")

    case _ => super.visit(lhs, rhs)
  }
}
