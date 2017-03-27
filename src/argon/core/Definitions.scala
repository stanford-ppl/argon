package argon.core

import argon.graphs._
import argon.utils.recursive
import argon.transform.Transformer
import org.virtualized.EmptyContext

trait Definitions extends Blocks { self: Staging =>
  type Tx = Transformer{val IR: self.type }

  /** Generalized Def representation which can have arbitrary output(s) -- roughly equivalent to LMS's FatDef **/
  abstract class Def extends Node with Product {

    final def allInputs: Seq[Dyn[_]] = nodeInputs(this.id).map{id => symFromSymId(id) }
    final def expInputs: Seq[Exp[_]] = recursive.collectSeqs(__exps)(productIterator)


    def outputTypes: Seq[Type[_]]

    /** Scheduling dependencies -- used to calculate schedule for IR based on dependencies **/
    // Inputs: symbol dataflow dependencies for this Def.
    // Default: All symbols in the Def's case class constructor AND scope (block/lambda) results
    def inputs: Seq[Dyn[_]] = recursive.collectSeqs(__dyns)(productIterator)

    // Reads: symbols *dereferenced* by this Def.
    // Default: All symbol inputs
    def reads: Seq[Dyn[_]] = inputs

    // Freqs: symbol frequency hints used in code motion - less than 0.75f is "cold", while greater than 100f is "hot"
    // Code motion makes an attempt to schedule unbound "hot" symbols early (move out of blocks)
    // Default: All symbol inputs have a frequency of 1.0f ("normal")
    def freqs: Seq[(Dyn[_],Float)] = Nil

    // Scopes: scopes associated with this Def
    // Default: All blocks and lambdas in the Def's case class constructor
    def blocks: Seq[Block[_]] = recursive.collectSeq{case b: Block[_] => b}(productIterator)

    // Binds: symbols "bound" by this Def
    // Bound symbols define the start of scopes. Effectful symbols in a scope typically must be bound.
    // All dependents of bound syms up until but not including the binding Def make up the majority of a scope
    // Default: All effects included in all scopes associated with this Def
    def binds: Seq[Dyn[_]] = blocks.flatMap(_.effectful)

    // Tunnels: symbols "bound" in scopes of this Def, but defined elsewhere
    // Tunnel symbols define the start of scopes, and are coupled with the result of the scope to avoid ambiguity
    // All paths from tunnel symbols to their corresponding results are effectively bound within the scope of this Def
    // Default: All inputs of all scopes associated with this Def, with the corresponding block results
    def tunnels: Seq[(Dyn[_],Seq[Dyn[_]])] = blocks.flatMap{blk =>
      val results = dyns(blk.result +: blk.effectful)
      blk.inputs.map{in => (in,results) }
    }.groupBy(_._1).mapValues(_.flatMap(_._2)).toSeq

    /** Alias hints -- used to check/disallow unsafe mutable aliasing **/
    // Aliases: inputs to this Def which *may* equal to the output of this Def
    // E.g. y = if (cond) a else b: aliases should return a and b
    // Default: All inputs which have the same type as an output
    def aliases: Seq[Dyn[_]] = recursive.collectSeq{case s: Dyn[_] if outputTypes.contains(s.tp) => s}(productIterator)

    // Contains: inputs which may be returned when dereferencing the output of this Def
    // E.g. y = Array(x): contains should return x
    // Default: no symbols
    def contains: Seq[Dyn[_]] = Nil

    // Extracts: inputs which, when dereferenced, may return the output of this Def
    // E.g. y = ArrayApply(x): extracts should return x
    // Default: no symbols
    def extracts: Seq[Dyn[_]] = Nil

    // Copies: inputs which, when dereferenced, may return the same pointer as dereferencing the output of this Def
    // E.g. y = ArrayCopy(x): copies should return x
    // Default: no symbols
    // TODO: In LMS, why is this different default than aliases?
    def copies: Seq[Dyn[_]] = Nil


    /** Mirroring and Mutating **/
    def mutate(f:Tx): Unit = throw new Exception("Cannot mutate immutable node")
    def fatMirror(f:Tx): Seq[Exp[_]]


    def updateNode(orig: Seq[Sym[_]], f: Tx): Seq[Exp[_]] = {
      try {
        mutate(f)
        orig
      }
      catch{case _:Throwable =>
        fatMirror(f)
      }
    }
    def mirrorNode(orig: Seq[Sym[_]], f: Tx): Seq[Exp[_]] = fatMirror(f)

    implicit val src: SrcCtx = EmptyContext
  }

  /** Most common variant of Def - returns only one symbol of one type **/
  abstract class Op[R:Type] extends Def {
    val mR = typ[R]

    def mirror(f:Tx): Exp[R]

    final override def outputTypes = List(mR)
    final override def fatMirror(f:Tx): List[Exp[_]] = List(this.mirror(f))
  }
  abstract class Op2[A:Type,R:Type] extends Op[R] { def mA = typ[A] }
  abstract class Op3[A:Type,B:Type,R:Type] extends Op2[A,R] { def mB = typ[B] }
  abstract class Op4[A:Type,B:Type,C:Type,R:Type] extends Op3[A,B,R] { def mC = typ[C] }
  abstract class Op5[A:Type,B:Type,C:Type,D:Type,R:Type] extends Op4[A,B,C,R] { def mD = typ[D] }

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

  private val __dyns: PartialFunction[Any,Seq[Dyn[_]]] = {
    case s: Dyn[_] => Seq(s)
    case b: Block[_]  => dyns(b.result)
    case d: Def       => d.inputs
    case l: Iterable[_] => recursive.collectSeqs(__dyns)(l.iterator)
  }
  private val __exps: PartialFunction[Any,Seq[Exp[_]]] = {
    case e: Exp[_]   => Seq(e)
    case b: Block[_] => Seq(b.result)
    case d: Def      => d.expInputs
    case l: Iterable[_] => recursive.collectSeqs(__exps)(l.iterator)
  }

  final def exps(a: Any*): Seq[Exp[_]] = if (__exps.isDefinedAt(a)) __exps(a) else Nil
  final def dyns(a: Any*): Seq[Dyn[_]] = if (__dyns.isDefinedAt(a)) __dyns(a) else Nil
  final def syms(a: Any*): Seq[Sym[_]] = dyns(a).collect{case s: Sym[_] => s}

  private def symsFreq(a: Any*): Seq[(Dyn[_],Float)] = recursive.collectSeqs {
    case s: Dyn[_] => Iterable((s, 1.0f))
    case b: Block[_]  => symsFreq(b.result)
    case d: Def       => d.freqs
  }(a)

  final def normal(e: Any*) = symsFreq(e:_*)
  final def hot(e: Any*) = symsFreq(e:_*).map{case (s,f) => (s,f*1000.0f) }
  final def cold(e: Any*) = symsFreq(e:_*).map{case (s,f) => (s, f*0.5f) }

  final def aliasSyms(a: Any): Set[Dyn[_]]   = recursive.collectSets{case s: Dyn[_] => Set(s) case d: Def => d.aliases }(a)
  final def containSyms(a: Any): Set[Dyn[_]] = recursive.collectSets{case d: Def => d.contains}(a)
  final def extractSyms(a: Any): Set[Dyn[_]] = recursive.collectSets{case d: Def => d.extracts}(a)
  final def copySyms(a: Any): Set[Dyn[_]]    = recursive.collectSets{case d: Def => d.copies}(a)
}
