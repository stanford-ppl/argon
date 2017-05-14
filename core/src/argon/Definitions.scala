package argon

import org.virtualized.{SourceContext, EmptyContext}
import forge._
import graphs._
import utils.recursive


/** Generalized Def representation which can have arbitrary output(s) -- roughly equivalent to LMS's FatDef **/
abstract class Def extends Node with Product {
  type Tx = argon.transform.Transformer

  @stateful final def allInputs: Seq[Dyn[_]] = state.graph.nodeInputs(this.id).map{id => symFromSymId(id) }
  final def expInputs: Seq[Exp[_]] = recursive.collectSeqs(__exps)(productIterator)

  def outputTypes: Seq[Type[_]]

  /** Scheduling dependencies -- used to calculate schedule for IR based on dependencies **/
  // Inputs: symbol dataflow dependencies for this Def.
  // Default: All symbols in the Def's case class constructor AND scope (block/lambda) results
  def inputs: Seq[Dyn[_]] = recursive.collectSeqs(__dyns)(productIterator)

  // Reads: symbols *dereferenced* by this Def.
  // Default: All symbol inputs
  def reads: Seq[Dyn[_]] = inputs

  // Freqs: symbol frequency hints used in code motion - frequency is either Freq.Hot, Freq.Cold, or Freq.Normal
  // Code motion makes an attempt to schedule unbound "hot" symbols early (move out of blocks)
  // Default: All symbol inputs have a frequency of Freq.Normal, block dependencies depend on Block temp
  def freqs: Seq[(Dyn[_],Freq)] = blocks.flatMap{blk => dyns(blk).map(_ -> blk.temp)}

  // Scopes: scopes associated with this Def
  // Default: All blocks and lambdas in the Def's case class constructor
  def blocks: Seq[Block[_]] = recursive.collectSeq{case b: Block[_] => b}(productIterator)

  // Binds: symbols "bound" by this Def
  // Bound symbols define the start of scopes. Effectful symbols in a scope typically must be bound.
  // All dependents of bound syms up until but not including the binding Def make up the majority of a scope
  // NOTE: Tempting to use productIterator here too, but note that Bound values can be inputs
  // Default: All effects included in all scopes associated with this Def
  def binds: Seq[Dyn[_]] = blocks.flatMap(_.effectful)

  // Tunnels: symbols "bound" in scopes of this Def, but defined elsewhere
  // Tunnel symbols define the start of scopes, and are coupled with the result of the scope to avoid ambiguity
  // All paths from tunnel symbols to their corresponding results are effectively bound within the scope of this Def
  // Default: All inputs of all scopes associated with this Def, with the corresponding block results
  def tunnels: Seq[(Dyn[_],Seq[Dyn[_]])] = blocks.flatMap{blk =>
    val results = dyns(blk.result +: blk.effectful)
    blk.inputs.collect{case in: Dyn[_] => (in,results) }
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

  implicit val src: SourceContext = EmptyContext
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

object Op {
  def unapply[T](e: Exp[T]): Option[Op[T]] = e match {
    case s: Sym[_] => defOf(s) match {
      case op: Op[_] => Some(op.asInstanceOf[Op[T]])
      case _ => None
    }
    case _ => None
  }
}


object Def {
  def unapply(e: Exp[_]): Option[Def] = e match {
    case s: Sym[_] => Some(defOf(s))
    case _ => None
  }
}


// --- Statements
case class Stm(lhs: Seq[Sym[_]], rhs: Def)

// "Typed pair" - symbol + an op
object TP {
  def unapply[A](x: Stm): Option[(Sym[_],Op[Any])] = x match {
    case Stm(List(lhs), rhs: Op[_]) => Some((lhs.asInstanceOf[Sym[_]], rhs.asInstanceOf[Op[Any]]))
    case _ => None
  }
}

// "Tupled type pair" - one or more symbols + a Def
object TTP {
  def unapply(x: Stm): Option[(Seq[Sym[_]],Def)] = x match {
    case Stm(_, rhs: Op[_]) => None
    case stm: Stm => Some((stm.lhs,stm.rhs))
    case _ => None
  }
}


