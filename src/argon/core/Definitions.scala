package argon.core

import argon.graphs._
import argon.utils.recursive
import argon.transform.Transformer
import org.virtualized.SourceContext

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.control.NoStackTrace

trait Definitions extends Blocks { self: Staging =>
  type Tx = Transformer{val IR: self.type }

  protected val here = implicitly[SourceContext]

  /** Generalized Def representation which can have arbitrary output(s) -- roughly equivalent to LMS's FatDef **/
  abstract class Def extends Node with Product {

    final def allInputs: List[Symbol[_]] = nodeInputs(this.id).map{id => symFromSymId(id) }

    def outputTypes: List[Staged[_]]

    /** Scheduling dependencies -- used to calculate schedule for IR based on dependencies **/
    // Inputs: symbol dataflow dependencies for this Def.
    // Default: All symbols in the Def's case class constructor AND scope (block/lambda) results
    def inputs: List[Symbol[_]] = recursive.collectLists(__syms)(productIterator)

    // Reads: symbols *dereferenced* by this Def.
    // Default: All symbol inputs
    def reads: List[Symbol[_]] = inputs

    // Freqs: symbol frequency hints used in code motion - less than 0.75f is "cold", while greater than 100f is "hot"
    // Code motion makes an attempt to schedule unbound "hot" symbols early (move out of blocks)
    // Default: All symbol inputs have a frequency of 1.0f ("normal")
    def freqs: List[(Symbol[_],Float)] = Nil

    // Scopes: scopes associated with this Def
    // Default: All blocks and lambdas in the Def's case class constructor
    def blocks: List[Block[_]] = recursive.collectList{case b: Block[_] => b}(productIterator)

    // Binds: symbols "bound" by this Def
    // Bound symbols define the start of scopes. Effectful symbols in a scope typically must be bound.
    // Dataflow dependents of bound syms up until but not including the binding Def make up the majority of a scope
    // Default: All effects included in all scopes associated with this Def
    def binds: List[Symbol[_]] = blocks.flatMap(_.effectful)

    // Tunnels: symbols "bound" in scopes of this Def, but defined elsewhere
    // Tunnel symbols define the start of scopes.
    def tunnels: List[Symbol[_]] = Nil

    /** Alias hints -- used to check/disallow unsafe mutable aliasing **/
    // Aliases: inputs to this Def which *may* equal to the output of this Def
    // E.g. y = if (cond) a else b: aliases should return a and b
    // Default: All inputs which have the same type as an output
    def aliases: List[Symbol[_]] = recursive.collectList{case s: Symbol[_] if outputTypes.contains(s.tp) => s}(productIterator)

    // Contains: inputs which may be returned when dereferencing the output of this Def
    // E.g. y = Array(x): contains should return x
    // Default: no symbols
    def contains: List[Symbol[_]] = Nil

    // Extracts: inputs which, when dereferenced, may return the output of this Def
    // E.g. y = ArrayApply(x): extracts should return x
    // Default: no symbols
    def extracts: List[Symbol[_]] = Nil

    // Copies: inputs which, when dereferenced, may return the same pointer as dereferencing the output of this Def
    // E.g. y = ArrayCopy(x): copies should return x
    // Default: no symbols
    // TODO: In LMS, why is this different default than aliases?
    def copies: List[Symbol[_]] = Nil


    /** Mirroring and Mutating **/
    def mutate(f:Tx): Unit = throw new Exception("Cannot mutate immutable node")
    def fatMirror(f:Tx): List[Exp[_]]


    def updateNode(orig:List[Sym[_]],f:Tx): List[Exp[_]] = {
      try {
        mutate(f)
        orig
      }
      catch{case _:Throwable =>
        fatMirror(f)
      }
    }
    def mirrorNode(orig: List[Sym[_]], f:Tx): List[Exp[_]] = fatMirror(f)

    implicit val src: SrcCtx = here
  }

  /** Most common variant of Def - returns only one symbol of one type **/
  abstract class Op[R:Staged] extends Def {
    def mirror(f:Tx): Exp[R]

    final override def outputTypes = List(implicitly[Staged[R]])
    final override def fatMirror(f:Tx): List[Exp[_]] = List(this.mirror(f))
    def mR = typ[R]
  }
  abstract class Op2[A:Staged,R:Staged] extends Op[R] { def mA = typ[A] }
  abstract class Op3[A:Staged,B:Staged,R:Staged] extends Op2[A,R] { def mB = typ[B] }
  abstract class Op4[A:Staged,B:Staged,C:Staged,R:Staged] extends Op3[A,B,R] { def mC = typ[C] }
  abstract class Op5[A:Staged,B:Staged,C:Staged,D:Staged,R:Staged] extends Op4[A,B,C,R] { def mD = typ[D] }

  /** Api **/
  object Def {
    def unapply(e: Exp[_]): Option[Def] = e match {
      case s: Sym[_] => Some(defOf(s))
      case _ => None
    }
  }
  object Op {
    def unapply[T](e: Exp[T]): Option[Op[T]] = e match {
      case s: Sym[_] => defOf(s) match {
        case op: Op[_] => Some(op.asInstanceOf[Op[T]])
        case _ => None
      }
      case _ => None
    }
  }

  // --- Helper functions
  def defOf(s:Sym[_]): Def = defFromSymId(s.id).get
  def getDef(s: Exp[_]): Option[Def] = s match { case s: Sym[_] => Some(defOf(s)); case _ => None }

  private val __syms: PartialFunction[Any,List[Symbol[_]]] = {
    case s: Symbol[_] => List(s)
    case b: Block[_] => syms(b.result)
    case d: Def => d.inputs
    case l: Iterable[_] => recursive.collectLists(__syms)(l.iterator)
  }
  def syms(a: Any*): List[Symbol[_]] = {
    val result = if (__syms.isDefinedAt(a)) __syms(a) else Nil
    //log(c"syms($a) = $result")
    result
  }


  def onlySyms(a: Any*): Seq[Sym[_]] = syms(a).collect{case s: Sym[_] => s}


  private def symsFreq(a: Any): List[(Symbol[_],Float)] = recursive.collectLists {
    case s:Symbol[_] => Iterable((s, 1.0f))
    case d:Def => d.freqs
  }(a)

  final def normal(e: Any) = symsFreq(e)
  final def hot(e: Any) = symsFreq(e).map{case (s,f) => (s,f*1000.0f) }
  final def cold(e: Any) = symsFreq(e).map{case (s,f) => (s, f*0.5f) }

  final def aliasSyms(a: Any): Set[Symbol[_]]   = recursive.collectSets{case s: Symbol[_] => Set(s) case d: Def => d.aliases }(a)
  final def containSyms(a: Any): Set[Symbol[_]] = recursive.collectSets{case d: Def => d.contains}(a)
  final def extractSyms(a: Any): Set[Symbol[_]] = recursive.collectSets{case d: Def => d.extracts}(a)
  final def copySyms(a: Any): Set[Symbol[_]]    = recursive.collectSets{case d: Def => d.copies}(a)
}
