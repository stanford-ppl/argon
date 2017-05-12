package argon.core

import argon._
import argon.utils.escapeConst
import forge._

trait Staging { this: ArgonCore =>

  @stateful def fresh[T:Type]: Bound[T] = {
    val bnd = new Bound(typ[T])
    state.graph.addBound(bnd)
    bnd
  }
  @stateful def constant[T:Type](c: Any)(implicit ctx: SrcCtx): Const[T] = {
    val cc = new Const[T](c)(typ[T])
    log(c"Making constant ${typ[T]} from ${escapeConst(c)} : ${c.getClass}")
    state.graph.registerInput(cc)
    cc.setCtx(ctx)
    cc
  }
  @stateful def parameter[T:Type](c: Any)(implicit ctx: SrcCtx): Param[T] = {
    val pid = state.nextParamId()
    val p = new Param[T](c, pid)(typ[T])
    log(c"Making parameter ${typ[T]} from ${escapeConst(p)} : ${c.getClass}")
    state.graph.registerInput(p)
    p.setCtx(ctx)
    p
  }

  @stateful def stageDef(d: Def)(ctx: SrcCtx): Seq[Sym[_]]                   = stageDefPure(d)(ctx)
  @stateful def stageDefPure(d: Def)(ctx: SrcCtx): Seq[Sym[_]]               = stageDefEffectful(d, Pure)(ctx)
  @stateful def stageDefCold(d: Def)(ctx: SrcCtx): Seq[Sym[_]]               = stageDefEffectful(d, Cold)(ctx)
  @stateful def stageDefWrite(ss: Exp[_]*)(d: Def)(ctx: SrcCtx): Seq[Sym[_]] = stageDefEffectful(d, Write(ss:_*))(ctx)
  @stateful def stageDefSimple(d: Def)(ctx: SrcCtx): Seq[Sym[_]]             = stageDefEffectful(d, Simple)(ctx)
  @stateful def stageDefGlobal(d: Def)(ctx: SrcCtx): Seq[Sym[_]]             = stageDefEffectful(d, Global)(ctx)
  @stateful def stageDefMutable(d: Def)(ctx: SrcCtx): Seq[Sym[_]]            = stageDefEffectful(d, Mutable)(ctx)

  @stateful def stage[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                      = single[T](stageDef(op)(ctx))
  @stateful def stagePure[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefPure(op)(ctx))
  @stateful def stageCold[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefCold(op)(ctx))
  @stateful def stageWrite[T:Type](ss: Exp[_]*)(op: Op[T])(ctx: SrcCtx): Sym[T]    = single[T](stageDefWrite(ss:_*)(op)(ctx))
  @stateful def stageSimple[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefSimple(op)(ctx))
  @stateful def stageGlobal[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefGlobal(op)(ctx))
  @stateful def stageMutable[T:Type](op: Op[T])(ctx: SrcCtx): Sym[T]               = single[T](stageDefMutable(op)(ctx))
  @stateful def stageEffectful[T:Type](op: Op[T], u: Effects)(ctx: SrcCtx): Sym[T] = single[T](stageDefEffectful(op, u)(ctx))

  @stateful private def registerDefWithCSE(d: Def)(ctx: SrcCtx): Seq[Sym[_]] = {
    // log(c"Checking defCache for $d")
    // log(c"Def cache: " + defCache.map{case (d,ss) => c"$d -> $ss"}.mkString("\n"))
    val syms = state.defCache.get(d) match {
      case Some(ss) => ss.foreach(_.addCtx(ctx)); ss
      case None => registerDef(d, Nil)(ctx)
    }
    state.defCache += d -> syms
    syms
  }

  @stateful private def registerDef(d: Def, extraDeps: Seq[Sym[_]])(ctx: SrcCtx): Seq[Sym[Any]] = {
    val bounds = d.binds
    val tunnels = d.tunnels
    val dfreqs = d.freqs.groupBy(_._1).mapValues(_.map(_._2).fold(Freq.Normal){Freq.combine})
    val freqs = d.inputs.map { in => dfreqs.getOrElse(in, Freq.Normal) } ++ extraDeps.distinct.map { d => Freq.Normal }

    val inputs = d.inputs
    val outputs = d.outputTypes.map{tp => new Sym(tp) }

    state.graph.addNode(inputs, outputs, bounds, tunnels, freqs, d)

    log(c"Staging node $outputs = $d")
    log(c"  schedule deps = $extraDeps")
    log(c"  inputs = $inputs")
    log(c"  tunnels = $tunnels")
    log(c"  binds = $bounds")
    log(c"  freqs = $freqs")

    outputs.foreach(_.setCtx(ctx))
    outputs
  }

  @stateful def stageDefEffectful(d: Def, u: Effects)(ctx: SrcCtx): Seq[Sym[_]] = {
    val atomicEffects = propagateWrites(u)

    log(c"Staging $d, effects = $u")
    log(c"  mutable inputs = ${mutableInputs(d)}")
    log(c"  actual writes = ${atomicEffects.writes}")

    val effects = atomicEffects andAlso Read(mutableInputs(d))
    log(c"  full effects = $effects")
    log(c"  isIdempotent = ${effects.isIdempotent}")

    if (effects == Pure) registerDefWithCSE(d)(ctx)
    else {
      state.checkContext()
      val deps = effectDependencies(effects)

      def stageEffects(): Seq[Sym[_]] = {
        val ss = registerDef(d, deps)(ctx)
        ss.foreach { s =>
          effectsOf(s) = effectsOf(s) andAlso effects
          depsOf(s) = depsOf(s) ++ deps
          state.context +:= s // prepend
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
        val symsWithSameDef = state.defCache.getOrElse(d, Nil) intersect state.context
        val symsWithSameEffects = symsWithSameDef.filter { case Effectful(u2, es) => u2 == effects && es == deps }

        if (symsWithSameEffects.isEmpty) {
          val syms = stageEffects()
          state.defCache += d -> syms
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
  @stateful def scrubSymbol(x: Sym[_]): Unit = {
    log(c"Scrubbing symbol $x from IR graph!")
    state.graph.removeEdge(x)
    state.context = state.context.filterNot(_ == x)
  }

}
