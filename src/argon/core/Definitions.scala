package argon.core

import argon.graphs._
import argon.utils.recursive
import argon.transform.Transformer
import virtualized.SourceContext

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.language.higherKinds

trait Definitions extends Blocks { self: Statements =>
  type Tx = Transformer{val IR: self.type }

  object Def {
    def unapply(s: Any): Option[Def] = s match {
      case s: Sym => defFromSymId(s.id) match { case _:NoDef[_] => None; case d => Some(d) }
      case _ => None
    }
  }

  def here = implicitly[SourceContext]

  private val rewriteRules = mutable.HashMap[Class[_], Queue[PartialFunction[Def, List[Sym]]]]()

  def rewriteFat[D<:Def:Manifest](func: PartialFunction[Any, List[Sym]]) = {
    val key = manifest[D].runtimeClass
    if (!rewriteRules.contains(key)) rewriteRules(key) = Queue.empty
    rewriteRules(key) :+= func.asInstanceOf[PartialFunction[Def,List[Sym]]]
  }

  def rewrite[O<:Op[_]:Manifest](func: PartialFunction[Any,Sym]) = rewriteFat[O](func andThen(k => List(k)) )

  //private val rewriteRules_noCtx = mutable.HashMap[Class[_], Queue[PartialFunction[Def, SrcCtx => List[Sym]]]]()

  /*def rewriteFatX[D<:Def:Manifest](func: PartialFunction[Any, SrcCtx => List[Sym]]) = {
    val key = manifest[D].runtimeClass
    if (!rewriteRules_noCtx.contains(key)) rewriteRules_noCtx(key) = Queue.empty
    rewriteRules_noCtx(key) :+= func.asInstanceOf[PartialFunction[Def,SrcCtx => List[Sym]]]
  }
  def rewriteX[O<:Op[_]:Manifest](func: PartialFunction[Any,SrcCtx => Sym]) = rewriteFatX[O](func andThen(_ andThen(k => List(k))) )
  */

  private val evalRules = mutable.HashMap[Class[_], Queue[PartialFunction[Def, List[Sym]]]]()
  def evalFat[D<:Def:Manifest](func: PartialFunction[D, List[Sym]]) = {
    val key = manifest[D].runtimeClass
    if (!evalRules.contains(key)) evalRules(key) = Queue.empty
    evalRules(key) :+= func.asInstanceOf[PartialFunction[Def,List[Sym]]]
  }
  def eval[O<:Op[_]:Manifest](func: PartialFunction[O,Sym]) = evalFat[O](func andThen(k => List(k)) )


  abstract class Def {
    def outputTypes: List[Typ[_]]

    /** Scheduling dependencies -- used to calculate schedule for IR based on dependencies **/
    // Inputs: symbol dataflow dependencies for this Def.
    // Default: All symbols in the Def's constructor AND block results
    var inputs: List[Sym] = recursive.collectList(syms)(this)

    // Reads: symbols *dereferenced* by this Def.
    // Default: All symbol inputs
    var reads: List[Sym] = inputs

    // Freqs: symbol frequency hints used in code motion - less than 0.75f is "cold", while greater than 100f is "hot"
    // Code motion makes an attempt to schedule "hot" symbols early (move out of blocks)
    // Default: All symbol inputs have a frequency of 1.0f ("normal")
    var freqs: List[(Sym,Float)] = Nil

    // Blocks: scopes associated with this Def
    // Default: All blocks in the Def's constructor
    var blocks: List[Block[_]] = recursive.collectList{case b: Block[_] => b}(this)

    // Binds: symbols "bound" by this Def
    // Bound symbols define the start of scopes. Effectful symbols in a scope typically must be bound.
    // Default: All effects included in all scopes associated with this Def
    var binds: List[Sym] = blocks.flatMap(_.effects)



    /** Alias hints -- used to check/disallow unsafe mutable aliasing **/

    // Aliases: inputs to this Def which *may* equal to the output of this Def
    // Default: all inputs to this symbol
    // TODO: Is this really the most sensible rule for aliasing?
    var aliases: List[Sym] = recursive.collectList(syms)(this)

    // Contains: inputs which may be returned when dereferencing the output of this Def
    // E.g. y = Array(x): contains should return x
    // Default: no symbols
    var contains: List[Sym] = Nil

    // Extracts: inputs which, when dereferenced, may return the output of this Def
    // E.g. y = ArrayApply(x): extracts should return x
    // Default: no symbols
    var extracts: List[Sym] = Nil

    // Copies: inputs which, when dereferenced, may return the same pointer as dereferencing the output of this Def
    // E.g. y = ArrayCopy(x): copies should return x
    // Default: no symbols
    var copies: List[Sym] = Nil


    /** Mirroring and Mutating **/
    def mutate(f:Tx): Unit = throw new Exception("Cannot mutate immutable node")

    def fatMirror(f:Tx): List[Sym]

    def updateNode(orig:List[Sym],f:Tx): List[Sym] = {
      try {
        mutate(f)
        orig
      }
      catch{case e:Throwable =>
        fatMirror(f)
      }
    }
    def mirrorNode(orig: List[Sym], f:Tx): List[Sym] = fatMirror(f)

    def rewriteOrElse(ss: => List[Sym]): List[Sym] = rewriteRules.get(this.getClass) match {
      case Some(rules) => rules.find(_.isDefinedAt(this)).map(_.apply(this)).getOrElse(ss)
      case None => ss
    }

    private[Definitions] var __id: Int = 0
    protected final implicit val here: SrcCtx = implicitly[SourceContext]
    final def id = this.__id
  }

  implicit object DefIsNodeLike extends NodeLike[Int,Def] {
    def setId(d: Def, id: Int): Unit = { d.__id = id }
    def getId(d: Def): Int = d.__id
  }


  case class NoDef[T:Typ]() extends Def {
    val outputTypes = List(typ[T])
    def fatMirror(f:Tx): List[Sym] = ???
    override def mirrorNode(orig: List[Sym], f:Tx): List[Sym] = orig
  }

  abstract class Op[R:Typ] extends Def {
    def mirror(f:Tx): R

    final override def outputTypes = List(implicitly[Typ[R]])
    final override def fatMirror(f:Tx): List[Sym] = List(this.mirror(f).asInstanceOf[Sym])
    final val mR = typ[R]
  }
  abstract class Op2[A:Typ,R:Typ] extends Op[R] { val mA = typ[A] }
  abstract class Op3[A:Typ,B:Typ,R:Typ] extends Op2[A,R] { val mB = typ[B] }
  abstract class Op4[A:Typ,B:Typ,C:Typ,R:Typ] extends Op3[A,B,R] { val mC = typ[C] }
  abstract class Op5[A:Typ,B:Typ,C:Typ,D:Typ,R:Typ] extends Op4[A,B,C,R] { val mD = typ[D] }

  private val syms: PartialFunction[Any,Sym] = {
    case s: Sym => s
    case Block(res: Sym,_,_) => res
  }

  private def symsFreq(a: Any): List[(Sym,Float)] = recursive.collectLists {
    case s:Sym => Iterable((s, 1.0f))
    case d:Def => d.freqs
  }(a)

  final def normal(e: Any) = symsFreq(e)
  final def hot(e: Any) = symsFreq(e).map{case (s,f) => (s,f*1000.0f) }
  final def cold(e: Any) = symsFreq(e).map{case (s,f) => (s, f*0.5f) }

  final def aliasSyms(a: Any): Set[Sym]   = recursive.collectSets{case s: Sym => Set(s) case d: Def => d.aliases }(a)
  final def containSyms(a: Any): Set[Sym] = recursive.collectSets{case d: Def => d.contains}(a)
  final def extractSyms(a: Any): Set[Sym] = recursive.collectSets{case d: Def => d.extracts}(a)
  final def copySyms(a: Any): Set[Sym]    = recursive.collectSets{case d: Def => d.copies}(a)
}
