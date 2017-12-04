package argon.core.cake

import argon.util.recursive
import forge._

trait LayerDefs { this: ArgonCake =>
  // --- Helper functions
  @stateful def defOf(s:Sym[_]): Def = defFromSymId(s.id).getOrElse{
    state.graph.dumpSymbolTable((lhs,rhs) => dbg(s"$lhs = $rhs"))
    throw new Exception(s"Attempted to get defOf Sym $s, but no Stm was found")
  }
  @stateful def getDef(s: Exp[_]): Option[Def] = s match { case s: Sym[_] => Some(defOf(s)); case _ => None }

  val __dyns: PartialFunction[Any,Seq[Dyn[_]]] = {
    case s: Dyn[_] => Seq(s)
    case b: Block[_]  => dyns(b.result) ++ dyns(b.effectful)
    case d: Def       => d.inputs
    case l: Iterable[_] => recursive.collectSeqs(__dyns)(l.iterator)
  }
  val __exps: PartialFunction[Any,Seq[Exp[_]]] = {
    case e: Exp[_]   => Seq(e)
    case b: Block[_] => Seq(b.result) ++ b.effectful
    case d: Def      => d.expInputs
    case l: Iterable[_] => recursive.collectSeqs(__exps)(l.iterator)
  }

  final def exps(a: Any*): Seq[Exp[_]] = if (__exps.isDefinedAt(a)) __exps(a) else Nil
  final def dyns(a: Any*): Seq[Dyn[_]] = if (__dyns.isDefinedAt(a)) __dyns(a) else Nil
  final def syms(a: Any*): Seq[Sym[_]] = dyns(a).collect{case s: Sym[_] => s}

  private def symsFreq(a: Any*): Seq[(Dyn[_],Freq)] = recursive.collectSeqs {
    case s: Dyn[_]    => Iterable((s, Freq.Normal))
    case b: Block[_]  => symsFreq(b.result) ++ symsFreq(b.effectful)
    case d: Def       => d.freqs
  }(a)

  final def normal(e: Any*) = symsFreq(e:_*)
  final def hot(e: Any*) = symsFreq(e:_*).map{case (s,f) => (s, Freq.combine(f, Freq.Hot)) }
  final def cold(e: Any*) = symsFreq(e:_*).map{case (s,f) => (s, Freq.combine(f, Freq.Cold)) }

  final def aliasSyms(a: Any): Set[Dyn[_]]   = recursive.collectSets{case s: Dyn[_] => Set(s) case d: Def => d.aliases }(a)
  final def containSyms(a: Any): Set[Dyn[_]] = recursive.collectSets{case d: Def => d.contains}(a)
  final def extractSyms(a: Any): Set[Dyn[_]] = recursive.collectSets{case d: Def => d.extracts}(a)
  final def copySyms(a: Any): Set[Dyn[_]]    = recursive.collectSets{case d: Def => d.copies}(a)

  implicit class DefOps(x: Def) {
    @stateful def allInputs(implicit state: State): Seq[Dyn[_]] = state.graph.nodeInputs(x.id).map{id => symFromSymId(id) }
  }

}
