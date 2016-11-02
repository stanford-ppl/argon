package argon.core

import argon.graphs._

import scala.collection.mutable
import virtualized.SourceContext

trait Statements extends Definitions with ArgonExceptions {
  override val graph: HDAG[Sym,Def,Metadata[_]]

  // -- State
  private val defCache = new mutable.HashMap[Def, List[Sym]]
  private val shallowAliasCache = new mutable.HashMap[Sym, Set[Sym]]
  private val deepAliasCache = new mutable.HashMap[Sym, Set[Sym]]
  private val aliasCache = new mutable.HashMap[Sym, Set[Sym]]

  // --- Statements
  case class Stm(lhs: List[Sym], rhs: Def)


  // --- Staging
  def fresh[T:Typ]:T = single[T](registerDef(NoDef[T](), Nil)(implicitly[SourceContext]))

  private def registerDefWithCSE(d:Def)(ctx:SrcCtx):List[Sym] = {
    // log(c"Checking defCache for $d")
    // log(c"Def cache: " + defCache.map{case (d,ss) => c"$d -> $ss"}.mkString("\n"))
    val syms = defCache.get(d) match {
      case Some(ss) => ss.map(_.withCtx(ctx))
      case None => registerDef(d, Nil)(ctx)
    }
    defCache(d) = syms
    syms
  }
  private def registerDef(d:Def, extraDeps:List[Sym])(ctx:SrcCtx): List[Sym] = d.rewriteOrElse({
    val outputs = d.outputTypes.map(_.next)
    val bounds = d.binds
    val freqs = d.freqs.groupBy(_._1).mapValues(_.map(_._2).sum)
    val inputs = d.inputs.map { in => (in, freqs.getOrElse(in, 1.0f)) } ++ extraDeps.distinct.map { d => (d, 1.0f) }

    graph.addNode(inputs, outputs, bounds, d)

    log(c"Staging node $d")
    log(c"  outputs = $outputs")
    log(c"  binds = $bounds")
    log(c"  freqs = $inputs")

    outputs
  }).map(_.setCtx(ctx))

  def stageDefEffectful(d:Def, u:Effects)(ctx:SrcCtx):List[Sym] = {
    log(c"Staging $d, effects = $u")

    val effects = u andAlso Read(mutableInputs(d))

    if (effects == Pure) registerDefWithCSE(d)(ctx)
    else {
      checkContext()
      val deps = effectDependencies(effects)

      def stageEffects(canCSE:Boolean):List[Sym] = {
        val ss = if (canCSE) registerDefWithCSE(d)(ctx) else registerDef(d, deps)(ctx)
        ss.foreach { s =>
          effectsOf(s) = effectsOf(s) andAlso effects
          depsOf(s) = depsOf(s) ++ deps
          context +:= s // prepend (O(1))
        }
        ss
      }
      if (effects.isIdempotent) {
        // CSE statements which are idempotent and have identical effect summaries (e.g. only read)
        val symsWithSameDef = defCache.getOrElse(d, Nil) intersect context
        val symsWithSameEffects = symsWithSameDef.filter { case Effectful(u2, es) => u2 == effects && es == deps }

        if (symsWithSameEffects.isEmpty) stageEffects(canCSE = true)
        else symsWithSameEffects.map(_.withCtx(ctx))
      }
      else {
        val z = stageEffects(canCSE = false)
        // Correctness checks -- cannot have mutable aliases, cannot mutate immutable symbols
        val aliases = mutableAliases(d) diff effects.writes
        val immutables = effects.writes.filterNot(isMutable)

        if (aliases.nonEmpty) throw IllegalMutableSharingError(z.head, aliases)(ctx)
        if (immutables.nonEmpty) throw IllegalMutationError(z.head, immutables)(ctx)
        z
      }
    }
  }
  def stageDef(d: Def)(ctx: SrcCtx): List[Sym]                = stageDefPure(d)(ctx)
  def stageDefPure(d: Def)(ctx: SrcCtx): List[Sym]            = stageDefEffectful(d, Pure)(ctx)
  def stageDefWrite(ss: Sym*)(d: Def)(ctx: SrcCtx): List[Sym] = stageDefEffectful(d, Write(ss.toSet))(ctx)
  def stageDefSimple(d: Def)(ctx: SrcCtx): List[Sym]          = stageDefEffectful(d, Simple)(ctx)
  def stageDefMutable(d: Def)(ctx: SrcCtx): List[Sym]         = stageDefEffectful(d, Mutable)(ctx)

  def stage[T:Typ](op: Op[T])(ctx: SrcCtx): T                   = single[T](stageDef(op)(ctx))
  def stagePure[T:Typ](op: Op[T])(ctx: SrcCtx): T               = single[T](stageDefPure(op)(ctx))
  def stageWrite[T:Typ](ss: Sym*)(op: Op[T])(ctx: SrcCtx): T    = single[T](stageDefWrite(ss:_*)(op)(ctx))
  def stageSimple[T:Typ](op: Op[T])(ctx: SrcCtx): T             = single[T](stageDefSimple(op)(ctx))
  def stageMutable[T:Typ](op: Op[T])(ctx: SrcCtx): T            = single[T](stageDefMutable(op)(ctx))
  def stageEffectful[T:Typ](op: Op[T], u: Effects)(ctx: SrcCtx) = single[T](stageDefEffectful(op, u)(ctx))


  // --- Helper functions
  private def single[T:Typ](xx: List[Sym]): T = xx.head.asInstanceOf[T]

  private[core] def stmFromNodeId(id: Int): Stm = {val x = graph.triple(id); Stm(x._1.toList, x._2) }
  private[core] def stmFromSymId(id: Int): Stm  = {val node = graph.producerOf(id); stmFromNodeId(node)}
  private[core] def symFromSymId(id: Int): Sym  = graph.edgeOf(id)
  private[core] def defFromNodeId(id: Int): Def = graph.nodeOf(id)
  private[core] def defFromSymId(id: Int): Def  = defFromNodeId(graph.producerOf(id))

  def stmOf(sym: Sym) = stmFromSymId(sym.id) match {
    case Stm(_, _:NoDef[_]) => None
    case stm => Some(stm)
  }
  def defOf(s:Sym):Option[Def] = Def.unapply(s)
  def hasDef(x:Sym) = Def.unapply(x).isDefined

  object Const {
    def unapply(s:Any):Option[Any] = s match {
      case s:Sym if s.isConst => s.getValue
      case _ => None
    }
  }

  object Param {
    def unapply(s:Any):Option[Any] = s match {
      case s:Sym => s.getValue
      case _ => None
    }
  }

  // --- Symbol aliasing
  private def noPrims(x:Set[Sym]) = x.filterNot { s => s.tp.isPrimitive || !hasDef(s) }

  def shallowAliases(x: Any): Set[Sym] = {
    noPrims(aliasSyms(x)).flatMap { case s@Def(d) => shallowAliasCache.getOrElseUpdate(s, shallowAliases(d)) + s } ++
      noPrims(containSyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) }
  }
  def deepAliases(x: Any): Set[Sym] = {
    noPrims(aliasSyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) } ++
      noPrims(copySyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) } ++
      noPrims(extractSyms(x)).flatMap { case s@Def(d) => aliasCache.getOrElseUpdate(s, allAliases(d)) + s } ++
      noPrims(containSyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) }
  }
  final def allAliases(x: Any): Set[Sym] = {
    shallowAliases(x) ++ deepAliases(x)
  }
  final def mutableAliases(x: Any): Set[Sym] = allAliases(x).filter(isMutable)
  final def mutableInputs(d: Def): Set[Sym] = {
    val bounds = d.binds
    val actuallyReadSyms = d.reads diff bounds
    mutableAliases(actuallyReadSyms) filterNot (bounds contains _)
  }

}
