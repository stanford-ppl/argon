package argon.core

import argon.graphs._
import argon.utils.recursive
import argon.transform.Transformer
import scala.virtualized.SourceContext

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.control.NoStackTrace

trait Definitions extends Blocks { self: Statements =>
  type Tx = Transformer{val IR: self.type }

  protected val here = implicitly[SourceContext]

  private val rewriteRules = mutable.HashMap[Class[_], Queue[PartialFunction[Def, List[Sym[_]]]]]()

  def rewriteFat[D<:Def:Manifest](func: PartialFunction[Any, List[Sym[_]]]) = {
    val key = manifest[D].runtimeClass
    if (!rewriteRules.contains(key)) rewriteRules(key) = Queue.empty
    rewriteRules(key) :+= func.asInstanceOf[PartialFunction[Def,List[Sym[_]]]]
  }

  def rewrite[O<:Op[_]:Manifest](func: PartialFunction[Any,Sym[_]]) = rewriteFat[O](func andThen(k => List(k)) )

  private val evalRules = mutable.HashMap[Class[_], Queue[PartialFunction[Def, List[Sym[_]]]]]()
  def evalFat[D<:Def:Manifest](func: PartialFunction[D, List[Sym[_]]]) = {
    val key = manifest[D].runtimeClass
    if (!evalRules.contains(key)) evalRules(key) = Queue.empty
    evalRules(key) :+= func.asInstanceOf[PartialFunction[Def,List[Sym[_]]]]
  }
  def eval[O<:Op[_]:Manifest](func: PartialFunction[O,Sym[_]]) = evalFat[O](func andThen(k => List(k)) )


  /** Generalized Def representation which can have arbitrary output(s) -- roughly equivalent to LMS's FatDef **/
  abstract class Def extends Node with Product {
    def outputTypes: List[Staged[_]]

    /** Scheduling dependencies -- used to calculate schedule for IR based on dependencies **/
    // Inputs: symbol dataflow dependencies for this Def.
    // Default: All symbols in the Def's constructor AND block results
    var inputs: List[Sym[_]] = recursive.collectLists(__syms)(productIterator)

    // Reads: symbols *dereferenced* by this Def.
    // Default: All symbol inputs
    var reads: List[Sym[_]] = inputs

    // Freqs: symbol frequency hints used in code motion - less than 0.75f is "cold", while greater than 100f is "hot"
    // Code motion makes an attempt to schedule unbound "hot" symbols early (move out of blocks)
    // Default: All symbol inputs have a frequency of 1.0f ("normal")
    var freqs: List[(Sym[_],Float)] = Nil

    // Blocks: scopes associated with this Def
    // Default: All blocks in the Def's constructor
    var blocks: List[Block[_]] = recursive.collectList{case b: Block[_] => b}(productIterator)

    // Binds: symbols "bound" by this Def
    // Bound symbols define the start of scopes. Effectful symbols in a scope typically must be bound.
    // Dataflow dependents of bound syms up until but not including the binding Def make up the majority of a scope
    // Default: All effects included in all scopes associated with this Def
    var binds: List[Sym[_]] = blocks.flatMap(_.effects)



    /** Alias hints -- used to check/disallow unsafe mutable aliasing **/
    // Aliases: inputs to this Def which *may* equal to the output of this Def
    // Default: all inputs to this symbol
    // TODO: Is this really the most sensible rule for aliasing?
    var aliases: List[Sym[_]] = recursive.collectLists(__syms)(productIterator)

    // Contains: inputs which may be returned when dereferencing the output of this Def
    // E.g. y = Array(x): contains should return x
    // Default: no symbols
    var contains: List[Sym[_]] = Nil

    // Extracts: inputs which, when dereferenced, may return the output of this Def
    // E.g. y = ArrayApply(x): extracts should return x
    // Default: no symbols
    var extracts: List[Sym[_]] = Nil

    // Copies: inputs which, when dereferenced, may return the same pointer as dereferencing the output of this Def
    // E.g. y = ArrayCopy(x): copies should return x
    // Default: no symbols
    var copies: List[Sym[_]] = Nil


    /** Mirroring and Mutating **/
    def mutate(f:Tx): Unit = throw new Exception("Cannot mutate immutable node") with NoStackTrace
    def fatMirror(f:Tx): List[Sym[_]]


    def updateNode(orig:List[Sym[_]],f:Tx): List[Sym[_]] = {
      try {
        mutate(f)
        orig
      }
      catch{case _:Throwable =>
        fatMirror(f)
      }
    }
    def mirrorNode(orig: List[Sym[_]], f:Tx): List[Sym[_]] = fatMirror(f)

    final def rewriteOrElse(ss: => List[Sym[_]]): List[Sym[_]] = rewriteRules.get(this.getClass) match {
      case Some(rules) => rules.find(_.isDefinedAt(this)).map(_.apply(this)).getOrElse(ss)
      case None => ss
    }

    implicit val src: SrcCtx = here
  }

  /** Most common variant of Def - returns only one symbol of one type **/
  abstract class Op[R:Staged] extends Def {
    def mirror(f:Tx): R

    final override def outputTypes = List(implicitly[Staged[R]])
    final override def fatMirror(f:Tx): List[Sym[_]] = List(this.mirror(f).asInstanceOf[Sym[_]])
    val mR = stg[R]
  }
  abstract class Op2[A:Staged,R:Staged] extends Op[R] { val mA = stg[A] }
  abstract class Op3[A:Staged,B:Staged,R:Staged] extends Op2[A,R] { val mB = stg[B] }
  abstract class Op4[A:Staged,B:Staged,C:Staged,R:Staged] extends Op3[A,B,R] { val mC = stg[C] }
  abstract class Op5[A:Staged,B:Staged,C:Staged,D:Staged,R:Staged] extends Op4[A,B,C,R] { val mD = stg[D] }

  /** Specialized "No-op" Defs **/
  case class NoOp[T:Staged]() extends Def {
    val outputTypes = List(stg[T])
    def fatMirror(f:Tx): List[Sym[_]] = throw new Exception("Cannot mirror NoDefs")
    override def mirrorNode(orig: List[Sym[_]], f:Tx): List[Sym[_]] = orig
  }

  // case class BoundSymbol[T:Staged]() extends NoOp[T]
  // case class Constant[T](staged: Staged[T], c: Any) extends NoOp[T]()(staged)
  // case class Parameter[T](staged: Staged[T], c: Any) extends NoOp[T]()(staged)


  /** Api **/
  object Def {
    def unapply(s: Sym[_]): Option[Def] = defOf(s) match {
      case _:NoOp[_] => None
      case d => Some(d)
    }
  }

  // --- Helper functions
  private[core] def defOf(s:Sym[_]): Def = defFromSymId(s.id)
  private[core] def hasDef(x:Sym[_]): Boolean = Def.unapply(x).isDefined

  private val __syms: PartialFunction[Any,List[Sym[_]]] = {
    case s: Sym[_] => List(s)
    case Block(res: Sym[_],_,_) => List(res)
    case d: Def => d.inputs
  }
  def syms(a: Any): List[Sym[_]] = if (__syms.isDefinedAt(a)) __syms(a) else Nil


  private def symsFreq(a: Any): List[(Sym[_],Float)] = recursive.collectLists {
    case s:Sym[_] => Iterable((s, 1.0f))
    case d:Def => d.freqs
  }(a)

  final def normal(e: Any) = symsFreq(e)
  final def hot(e: Any) = symsFreq(e).map{case (s,f) => (s,f*1000.0f) }
  final def cold(e: Any) = symsFreq(e).map{case (s,f) => (s, f*0.5f) }

  final def aliasSyms(a: Any): Set[Sym[_]]   = recursive.collectSets{case s: Sym[_] => Set(s) case d: Def => d.aliases }(a)
  final def containSyms(a: Any): Set[Sym[_]] = recursive.collectSets{case d: Def => d.contains}(a)
  final def extractSyms(a: Any): Set[Sym[_]] = recursive.collectSets{case d: Def => d.extracts}(a)
  final def copySyms(a: Any): Set[Sym[_]]    = recursive.collectSets{case d: Def => d.copies}(a)
}
