package argon.core

import argon.utils.escapeConst

trait Staging extends Scheduling {

  def fresh[T:Type]: Bound[T] = {
    val bnd = new Bound(typ[T])
    addBound(bnd)
    bnd
  }
  def constant[T:Type](c: Any)(implicit ctx: SrcCtx): Const[T] = {
    val cc = new Const[T](c)(typ[T])
    log(c"Making constant ${typ[T]} from ${escapeConst(c)} : ${c.getClass}")
    registerInput(cc)
    cc.setCtx(ctx)
    cc
  }
  def parameter[T:Type](c: Any)(implicit ctx: SrcCtx): Param[T] = {
    val p = new Param[T](c)(typ[T])
    log(c"Making parameter ${typ[T]} from ${escapeConst(p)} : ${c.getClass}")
    registerInput(p)
    p.setCtx(ctx)
    p
  }

  def stageDef(d: Def)(ctx: SrcCtx): Seq[Sym[_]]                   = stageDefPure(d)(ctx)
  def stageDefPure(d: Def)(ctx: SrcCtx): Seq[Sym[_]]               = stageDefEffectful(d, Pure)(ctx)
  def stageDefCold(d: Def)(ctx: SrcCtx): Seq[Sym[_]]               = stageDefEffectful(d, Cold)(ctx)
  def stageDefWrite(ss: Exp[_]*)(d: Def)(ctx: SrcCtx): Seq[Sym[_]] = stageDefEffectful(d, Write(ss:_*))(ctx)
  def stageDefSimple(d: Def)(ctx: SrcCtx): Seq[Sym[_]]             = stageDefEffectful(d, Simple)(ctx)
  def stageDefGlobal(d: Def)(ctx: SrcCtx): Seq[Sym[_]]             = stageDefEffectful(d, Global)(ctx)
  def stageDefMutable(d: Def)(ctx: SrcCtx): Seq[Sym[_]]            = stageDefEffectful(d, Mutable)(ctx)

  def stage[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                      = single[T](stageDef(op)(ctx))
  def stagePure[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefPure(op)(ctx))
  def stageCold[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefCold(op)(ctx))
  def stageWrite[T:Type](ss: Exp[_]*)(op: Op[T])(ctx: SrcCtx): Sym[T]    = single[T](stageDefWrite(ss:_*)(op)(ctx))
  def stageSimple[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefSimple(op)(ctx))
  def stageGlobal[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefGlobal(op)(ctx))
  def stageMutable[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]               = single[T](stageDefMutable(op)(ctx))
  def stageEffectful[T:Type](op: Op[T], u: Effects)(ctx: SrcCtx): Sym[T] = single[T](stageDefEffectful(op, u)(ctx))

  private def registerDefWithCSE(d: Def)(ctx: SrcCtx): Seq[Sym[_]] = {
    // log(c"Checking defCache for $d")
    // log(c"Def cache: " + defCache.map{case (d,ss) => c"$d -> $ss"}.mkString("\n"))
    val syms = defCache.get(d) match {
      case Some(ss) => ss.foreach(_.addCtx(ctx)); ss
      case None => registerDef(d, Nil)(ctx)
    }
    defCache(d) = syms
    syms
  }

  private def registerDef(d: Def, extraDeps: Seq[Sym[_]])(ctx: SrcCtx): Seq[Sym[Any]] = {
    val bounds = d.binds
    val tunnels = d.tunnels
    val dfreqs = d.freqs.groupBy(_._1).mapValues(_.map(_._2).sum)
    val freqs = d.inputs.map { in => dfreqs.getOrElse(in, 1.0f) } ++ extraDeps.distinct.map { d => 1.0f }

    val inputs = d.inputs
    val outputs = d.outputTypes.map{tp => new Sym(tp) }

    addNode(inputs, outputs, bounds, tunnels, freqs, d)

    log(c"Staging node $outputs = $d")
    log(c"  schedule deps = $extraDeps")
    log(c"  inputs = $inputs")
    log(c"  tunnels = $tunnels")
    log(c"  binds = $bounds")
    log(c"  freqs = $freqs")

    outputs.foreach(_.setCtx(ctx))
    outputs
  }

  def stageDefEffectful(d: Def, u: Effects)(ctx: SrcCtx): Seq[Sym[_]] = {
    val atomicEffects = propagateWrites(u)

    log(c"Staging $d, effects = $u")
    log(c"  mutable inputs = ${mutableInputs(d)}")
    log(c"  actual writes = ${atomicEffects.writes}")

    val effects = atomicEffects andAlso Read(mutableInputs(d))
    log(c"  full effects = $effects")
    log(c"  isIdempotent = ${effects.isIdempotent}")

    if (effects == Pure) registerDefWithCSE(d)(ctx)
    else {
      checkContext()
      val deps = effectDependencies(effects)

      def stageEffects(): Seq[Sym[_]] = {
        val ss = registerDef(d, deps)(ctx)
        ss.foreach { s =>
          effectsOf(s) = effectsOf(s) andAlso effects
          depsOf(s) = depsOf(s) ++ deps
          context +:= s // prepend
        }

        // Correctness checks -- cannot have mutable aliases, cannot mutate immutable symbols
        val immutables = effects.writes.filterNot(isMutable)
        val aliases = mutableAliases(d) diff effects.writes

        if (aliases.nonEmpty) new IllegalMutableSharingError(ss.head, aliases)(ctx)
        if (immutables.nonEmpty) new IllegalMutationError(ss.head, immutables)(ctx)

        ss
      }

      if (effects.isIdempotent) {
        // CSE statements which are idempotent and have identical effect summaries (e.g. repeated reads w/o writes)
        val symsWithSameDef = defCache.getOrElse(d, Nil) intersect context
        val symsWithSameEffects = symsWithSameDef.filter { case Effectful(u2, es) => u2 == effects && es == deps }

        if (symsWithSameEffects.isEmpty) {
          val syms = stageEffects()
          defCache(d) = syms
          syms
        }
        else {
          symsWithSameEffects.foreach(_.addCtx(ctx))
          symsWithSameEffects
        }
      }
      else stageEffects()
    }
  }

  private def single[T:Type](xx: Seq[Sym[_]]): Sym[T] = xx.head.asInstanceOf[Sym[T]]

  /**
    * DANGER ZONE
    * Use these methods only if you know what you're doing! (i.e. your name is David and you're not drunk)
    */
  def scrubSymbol(x: Sym[_]): Unit = {
    log(c"Scrubbing symbol $x from IR graph!")
    removeEdge(x)
    context = context.filterNot(_ == x)
  }

}
