package argon.analysis

import argon.core.Staging
import argon.traversal.Traversal

trait IndexPatternApi extends IndexPatternExp
trait IndexPatternExp extends Staging {
  type Index
  // Variations used here allow Index to be abstract (otherwise can't as easily define stride of 1)
  sealed abstract class IndexPattern { def index: Option[Bound[Index]] }
  // a*i + b, where a and b must be loop invariant
  case class AffineAccess(a: Exp[Index], i: Bound[Index], b: Exp[Index]) extends IndexPattern { def index = Some(i) }
  // i + b, where b must be loop invariant
  case class OffsetAccess(i: Bound[Index], b: Exp[Index]) extends IndexPattern { def index = Some(i) }
  // a*i, where a must be loop invariant
  case class StridedAccess(a: Exp[Index], i: Bound[Index]) extends IndexPattern { def index = Some(i) }
  // linear access with some loop iterator
  case class LinearAccess(i: Bound[Index]) extends IndexPattern { def index = Some(i) }
  // loop invariant access (but may change with outer loops)
  case class InvariantAccess(b: Exp[Index]) extends IndexPattern { def index = None }
  // anything else
  case object RandomAccess extends IndexPattern { def index = None }

  case class AccessPattern(indices: Seq[IndexPattern]) extends Metadata[AccessPattern] {
    def mirror(f:Tx) = AccessPattern(indices.map{
      case AffineAccess(a,i,b) => AffineAccess(f(a),f(i).asInstanceOf[Bound[Index]],f(b))
      case OffsetAccess(i,b)   => OffsetAccess(f(i).asInstanceOf[Bound[Index]], f(b))
      case StridedAccess(a,i)  => StridedAccess(f(a),f(i).asInstanceOf[Bound[Index]])
      case LinearAccess(i)     => LinearAccess(f(i).asInstanceOf[Bound[Index]])
      case InvariantAccess(b)  => InvariantAccess(f(b))
      case RandomAccess        => RandomAccess
    })
  }

  object accessPatternOf {
    def apply(x: Exp[_]): Seq[IndexPattern] = {
      metadata[AccessPattern](x).map(_.indices).getOrElse{ throw new UndefinedAccessPatternException(x) }
    }
    def update(x: Exp[_], indices: Seq[IndexPattern]) { metadata.add(x, AccessPattern(indices)) }
  }
}

trait AccessPatternAnalyzer extends Traversal {
  val IR: IndexPatternExp
  import IR._

  // All loop indices encountered above the current scope
  var loopIndices = Set[Bound[Index]]()
  var boundIndexPatterns = Map[Exp[Index], Seq[IndexPattern]]()

  // The list of statements which can be scheduled prior to block traversals
  // (All statements in all scopes below this scope)
  // If a statement is not contained in this set, it must be loop invariant (because it was already scheduled)
  var innerScopes = Map[Exp[Index],Seq[Stm]]()

  // The exact scope of the current loop
  var loopScope: Seq[Stm] = Nil

  private def inLoop[T](indices: Seq[Bound[Index]])(blk: => T): T = {
    val inner = innerStms
    indices.foreach{i => innerScopes += i -> inner}

    val prevIndices = loopIndices
    loopIndices ++= indices
    val result = blk
    loopIndices = prevIndices
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

  def extractIndexPattern(x: Exp[Index]) = x match {
    case Plus(Times(LoopIndex(i), a), b) if isInvariant(a,i) && isInvariant(b,i) => AffineAccess(a,i,b) // i*a + b
    case Plus(Times(a, LoopIndex(i)), b) if isInvariant(a,i) && isInvariant(b,i) => AffineAccess(a,i,b) // a*i + b
    case Plus(b, Times(LoopIndex(i), a)) if isInvariant(a,i) && isInvariant(b,i) => AffineAccess(a,i,b) // b + i*a
    case Plus(b, Times(a, LoopIndex(i))) if isInvariant(a,i) && isInvariant(b,i) => AffineAccess(a,i,b) // b + a*i
    case Plus(LoopIndex(i), b) if isInvariant(b,i)  => OffsetAccess(i,b)                                // i + b
    case Plus(b, LoopIndex(i)) if isInvariant(b,i)  => OffsetAccess(i,b)                                // b + i
    case Times(LoopIndex(i), a) if isInvariant(a,i) => StridedAccess(a,i)                               // i*a
    case Times(a, LoopIndex(i)) if isInvariant(a,i) => StridedAccess(a,i)                               // a*i
    case LoopIndex(i) => LinearAccess(i)                                                                // i
    case b if isInvariantForAll(b) => InvariantAccess(b)                                                // b
    case _ => RandomAccess                                                                              // other
  }
  def extractAccessPatterns(xs: Seq[Exp[Index]]) = xs.flatMap{
    case x if boundIndexPatterns.contains(x) => boundIndexPatterns(x)
    case x => List(extractIndexPattern(x))
  }

  override protected def visit(lhs: Sym[_], rhs: Op[_]): Unit = lhs match {
    case LoopLevels(levels) =>
      dbgs(s"[Loop] $lhs = $rhs")
      dbgs(s"  Levels: ${levels.length} ")
      dbgs(s"  Current indices: $loopIndices")
      levels.foreach { case (indices, blocks) =>
        dbgs(s"  Traversing loop level with $indices")
        inLoop(indices) {
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
