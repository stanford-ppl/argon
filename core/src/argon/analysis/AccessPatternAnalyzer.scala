package argon.analysis

import argon.core._
import argon.compiler._
import argon.UndefinedAccessPatternException
import argon.traversal.Traversal
import forge._

sealed abstract class AffineFunction {
  type Tx = argon.transform.Transformer
  def mirror(f:Tx): AffineFunction
  def eval(f: Exp[Index] => Int): Int
  def getEval(f: PartialFunction[Exp[Index],Int]): Option[Int] = {
    val (c,g) = partialEval(f)
    if (g.isEmpty) Some(c) else None
  }
  def partialEval(f: PartialFunction[Exp[Index],Int]): (Int, Option[AffineFunction])
  def symbols: Seq[Exp[Index]]
  @internal def negate: AffineFunction
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

  def partialEval(f: PartialFunction[Exp[Index],Int]): (Int, Option[AffineFunction]) = {
    val parts = x.map{case Left(e) if f.isDefinedAt(e) => Some(f(e)); case Right(af) => af.getEval(f); case _ => None }
    val cs = parts.filter(_.isDefined)
    val c = cs.map(_.get).product
    val g = x.zip(parts).filter(_._2.isEmpty).map(_._1)
    val func = if (g.isEmpty) None else Some(new Prod(g))
    (c,func)
  }
  def symbols: Seq[Exp[Index]] = x.flatMap{
    case Left(x) => Seq(x)
    case Right(func) => func.symbols
  }

  @internal def negate: AffineFunction = {
    new Prod(Left(FixPt.int32s(-1)) +: x)
  }

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

  def partialEval(f: PartialFunction[Exp[Index],Int]): (Int, Option[AffineFunction]) = {
    val parts = x.map{case Left(e) if f.isDefinedAt(e) => Some(f(e)); case Right(af) => af.getEval(f); case _ => None }
    val cs = parts.filter(_.isDefined)
    val c = cs.map(_.get).sum
    val g = x.zip(parts).filter(_._2.isEmpty).map(_._1)
    val func = if (g.isEmpty) None else Some(new Sum(g))
    (c,func)
  }
  def symbols: Seq[Exp[Index]] = x.flatMap{
    case Left(x) => Seq(x)
    case Right(func) => func.symbols
  }

  @internal def negate: AffineFunction = new Sum(x.map{
    case Left(e) => Right(Prod(FixPt.int32s(-1),e))
    case Right(af) => Right(af.negate)
  })

  override def toString: String = x.map{case Left(e) => c"$e"; case Right(af) => af.toString}.mkString(" + ")
}
object Sum {
  def apply(x: Exp[Index]) = new Sum(Seq(Left(x)))
  def apply(x: Seq[Either[Exp[Index],AffineFunction]]) = new Sum(x)
}
case object One extends Prod(Nil) { override def toString: String = "One" }
case object Zero extends Sum(Nil) { override def toString: String = "Zero" }
case object Unknown {
  def apply(x: Exp[Index]): AffineFunction = Sum(Seq(Left(x)))
}
object SumOfProds {
  // TODO: Could distribute here in the case of Sum(Prod(x, Sum()), etc.
  def unapply(sum: Sum): Option[Seq[Either[Exp[Index],Prod]]] = sum.x.foldLeft(Some(Nil) : Option[Seq[Either[Exp[Index],Prod]]] ){
    case (Some(left), Left(c))              => Some(left :+ Left(c))
    case (Some(left), Right(SumOfProds(s))) => Some(left ++ s)
    case (Some(left), Right(p: Prod)) if !p.x.exists{case Right(_: Sum) => true; case _ => false} => Some(left :+ Right(p))
    case _ => None
  }
}

case class AffineProduct(a: AffineFunction, i: Exp[Index]) {
  @internal def negate: AffineProduct = AffineProduct(a.negate, i)
}

sealed abstract class IndexPattern {
  type Tx = argon.transform.Transformer
  def mirror(f:Tx): IndexPattern

  /** Returns the innermost (last) index which this access varies with.
    * If undefined, this index pattern is invariant to ALL loop iterators
    */
  def lastVariantIndex: Option[Exp[Index]]
}

// product(a)*i + sum(b), where all elements in a and b must be loop invariant relative to i
case class SymbolicAffine(sums: Seq[AffineProduct], offset: AffineFunction) extends IndexPattern {
  def mirror(f:Tx) = {
    val sums2 = sums.map{case AffineProduct(a,i) => AffineProduct(a.mirror(f),f(i)) }
    val offset2 = offset.mirror(f)
    SymbolicAffine(sums2, offset2)
  }
  def lastVariantIndex: Option[Exp[Index]] = sums.lastOption.map(_.i)
}

// Random access pattern invariant with all loop indices below lastIndex (possibly none)
case class RandomAccess(lastVariantIndex: Option[Exp[Index]]) extends IndexPattern {
  def mirror(f:Tx) = RandomAccess(f(lastVariantIndex))
}

case class AccessPattern(indices: Seq[IndexPattern]) extends Metadata[AccessPattern] {
  def mirror(f:Tx) = AccessPattern(indices.map(_.mirror(f)))
}

object LinearAccess {
  def apply(i: Bound[Index]): SymbolicAffine = SymbolicAffine(Seq(AffineProduct(One,i)), Zero)
}


@data object accessPatternOf {
  def apply(x: Exp[_]): Seq[IndexPattern] = accessPatternOf.get(x).getOrElse{ throw new UndefinedAccessPatternException(x) }
  def update(x: Exp[_], indices: Seq[IndexPattern]) { metadata.add(x, AccessPattern(indices)) }
  def get(x: Exp[_]): Option[Seq[IndexPattern]] = metadata[AccessPattern](x).map(_.indices)
}

trait AccessPatternAnalyzer extends Traversal {

  // All loop indices encountered above the current scope
  var loopIndices = List[Bound[Index]]()    // Innermost index is last
  var loopFromIndex = Map[Bound[Index], Exp[_]]()
  var boundIndexPatterns = Map[Exp[Index], IndexPattern]()

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
    loopIndices = loopIndices ++ indices
    loopFromIndex ++= indices.map{i => i -> loop}
    val result = blk
    loopIndices = prevIndices
    loopFromIndex = prevLoops
    result
  }
  override protected def visitBlock[S](block: Block[S]): Block[S] = {
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
  // Pair of symbols for nodes used in address calculation subtraction nodes
  def indexMinusApply(x: Exp[Index]): Option[(Exp[Index], Exp[Index])]
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
  // Memory being read + list of addresses (for N-D access) + dimension + stride + length
  //def vectorReadUnapply(x: Exp[_]): Option[(Exp[_], Seq[Exp[Index]], Int, Int, Int)]
  // Memory being written + list of addresses (for N-D access) + dimension + stride + length
  //def vectorWriteUnapply(x: Exp[_]): Option[(Exp[_], Seq[Exp[Index]], Int, Int, Int)]

  def indexUnapply(x: Exp[Index]): Option[Bound[Index]] = x match {
    case s: Bound[_] if loopIndices.contains(s.asInstanceOf[Bound[Index]]) => Some(s.asInstanceOf[Bound[Index]])
    case _ => None
  }

  object Minus       { def unapply(x: Exp[Index]) = indexMinusApply(x) }
  object Plus        { def unapply(x: Exp[Index]) = indexPlusUnapply(x) }
  object Times       { def unapply(x: Exp[Index]) = indexTimesUnapply(x) }
  object LoopLevels  { def unapply(x: Exp[_]) = loopUnapply(x) }
  object MemRead     { def unapply(x: Exp[_]) = readUnapply(x) }
  object MemWrite    { def unapply(x: Exp[_]) = writeUnapply(x) }
  //object VectorRead  { def unapply(x: Exp[_]) = vectorReadUnapply(x) }
  //object VectorWrite { def unapply(x: Exp[_]) = vectorWriteUnapply(x) }
  object LoopIndex   { def unapply(x: Exp[Index]) = indexUnapply(x) }

  def offsetOf(i: Bound[Index]): Option[Exp[Index]] = None
  def strideOf(i: Bound[Index]): Option[Exp[Index]] = None

  /**
    * Check if expression b is invariant with respect to loop index i
    * An expression b is invariant to loop index i if it is defined outside of the loop scope of i
    */
  def isInvariant(b: Exp[Index], i: Exp[Index]): Boolean = b match {
    case Const(_) => true
    case Param(_) => true
    case s: Sym[_] => !innerScopes(i).exists{stm => stm.lhs.contains(s)}
    case _ => false
  }
  def isInvariantForAll(b: Exp[Index]): Boolean = loopIndices.forall{i => isInvariant(b,i) }

  def extractIndexPattern(x: Exp[Index]): IndexPattern = {
    dbgs(c"Looking for affine access patterns from ${str(x)}")

    def extractPattern(x: Exp[Index]): Option[(Seq[AffineProduct],AffineFunction)] = x match {
      case Minus(a,b) => (extractPattern(a), extractPattern(b)) match {
        case (Some((is,c1)), Some((is2,c2))) =>
          implicit val ctx: SrcCtx = x.ctx
          val offset = Sum(Seq(Right(c1), Right(c2.negate)))
          val negis2 = is2.map{_.negate}
          Some((is ++ negis2, offset))
        case _ => None
      }

      case Plus(a,b) => (extractPattern(a), extractPattern(b)) match {
        case (Some((is1,c1)), Some((is2,c2))) =>
          val offset = Sum(Seq(Right(c1),Right(c2)))
          Some((is1 ++ is2, offset))
        case _ => None
      }

      case Times(LoopIndex(i), a) if isInvariant(a,i) =>    // i*a
        val stride = strideOf(i).map{s => Prod(a,s) }.getOrElse(Prod(a))
        val offset = offsetOf(i).map{o => Prod(a,o) }.getOrElse(Zero)
        Some(Seq(AffineProduct(stride,i)), offset)

      case Times(a, LoopIndex(i)) if isInvariant(a,i) =>
        val stride = strideOf(i).map{s => Prod(a,s) }.getOrElse(Prod(a))
        val offset = offsetOf(i).map{o => Prod(a,o) }.getOrElse(Zero)
        Some(Seq(AffineProduct(stride,i)), offset)  // a*i

      case LoopIndex(i) =>
        val stride = strideOf(i).map{s => Prod(s) }.getOrElse(One)
        val offset = offsetOf(i).map{o => Sum(o) }.getOrElse(Zero)
        Some(Seq(AffineProduct(stride,i)), offset)  // i

      case b => Some((Nil, Unknown(b))) // b - anything else
    }

    val pattern = extractPattern(x)

    dbgs(c"Extracted pattern: " + pattern.mkString(" + "))

    pattern.flatMap{p =>
      val products = p._1
      val offset = p._2
      val groupedProducts = products.groupBy(_.i)
                                    .mapValues{funcs => funcs.map(af => Right(af.a) )}
                                    .toList
                                    .sortBy{case (i, _) => loopIndices.indexOf(i) }
                                    .map{case (i, as) => AffineProduct(Sum(as),i) }

      val indices = groupedProducts.map(_.i)

      if (offset.symbols.forall{x => indices.forall{i => isInvariant(x,i) }}) {
        Some(SymbolicAffine(groupedProducts, offset))
      }
      else None

    }.getOrElse{
      val idxOfLastInvariant = loopIndices.lastIndexWhere{i => !isInvariant(x,i) }
      val lastVariantIndex = if (idxOfLastInvariant >= 0) Some(loopIndices(idxOfLastInvariant)) else None
      RandomAccess(lastVariantIndex)
    }
  }

  def extractAccessPatterns(xs: Seq[Exp[Index]]): Seq[IndexPattern] = xs.map{
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

    /*case VectorRead(mem, addresses, dim, stride, len) =>
      accessPatternOf(lhs) = addresses.zipWithIndex.map{case (addr,d) =>
        if (dim == d) {
          val lastIndex = loopIndices( loopIndices.lastIndexWhere{i => isInvariant(addr,i) } )
          VectorAccess(extractIndexPattern(addr),stride,len,lastIndex)
        }
        else extractIndexPattern(addr)
      }
      dbgs(s"[Vector Read] $lhs = $rhs")
      dbgs(s"  Memory: $mem")
      dbgs(s"  ND Address: $addresses")
      dbgs(s"  Current indices: $loopIndices")
      dbgs(s"  Access pattern: ${accessPatternOf(lhs)}")*/

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
