package argon.core
import argon.utils.escapeConst

trait Staging extends Statements {
  def fresh[T:Staged]: Bound[T] = {
    val bnd = __bound[T]
    addBound(bnd)
    bnd
  }
  def constant[T:Staged](c: Any)(implicit ctx: SrcCtx): Const[T] = {
    val cc = __const[T](c)
    log(c"Making constant ${typ[T]} from ${escapeConst(c)} : ${c.getClass}")
    registerInput(cc)
    cc.setCtx(ctx)
    cc
  }
  def parameter[T:Staged](c: Any)(implicit ctx: SrcCtx): Param[T] = {
    val p = __param[T](c)
    log(c"Making parameter ${typ[T]} from ${escapeConst(p)} : ${c.getClass}")
    registerInput(p)
    p.setCtx(ctx)
    p
  }

  def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.staged.wrapped(constant[B](x)(l.staged,ctx))

  def stageDef(d: Def)(ctx: SrcCtx): List[Sym[_]]                   = stageDefPure(d)(ctx)
  def stageDefPure(d: Def)(ctx: SrcCtx): List[Sym[_]]               = stageDefEffectful(d, Pure)(ctx)
  def stageDefCold(d: Def)(ctx: SrcCtx): List[Sym[_]]               = stageDefEffectful(d, Cold)(ctx)
  def stageDefWrite(ss: Exp[_]*)(d: Def)(ctx: SrcCtx): List[Sym[_]] = stageDefEffectful(d, Write(ss:_*))(ctx)
  def stageDefSimple(d: Def)(ctx: SrcCtx): List[Sym[_]]             = stageDefEffectful(d, Simple)(ctx)
  def stageDefGlobal(d: Def)(ctx: SrcCtx): List[Sym[_]]             = stageDefEffectful(d, Global)(ctx)
  def stageDefMutable(d: Def)(ctx: SrcCtx): List[Sym[_]]            = stageDefEffectful(d, Mutable)(ctx)

  def stage[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                      = single[T](stageDef(op)(ctx))
  def stagePure[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefPure(op)(ctx))
  def stageCold[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefCold(op)(ctx))
  def stageWrite[T:Staged](ss: Exp[_]*)(op: Op[T])(ctx: SrcCtx): Sym[T]    = single[T](stageDefWrite(ss:_*)(op)(ctx))
  def stageSimple[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefSimple(op)(ctx))
  def stageGlobal[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefGlobal(op)(ctx))
  def stageMutable[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]               = single[T](stageDefMutable(op)(ctx))
  def stageEffectful[T:Staged](op: Op[T], u: Effects)(ctx: SrcCtx): Sym[T] = single[T](stageDefEffectful(op, u)(ctx))

  private def registerDefWithCSE(d: Def)(ctx: SrcCtx): List[Sym[_]] = {
    // log(c"Checking defCache for $d")
    // log(c"Def cache: " + defCache.map{case (d,ss) => c"$d -> $ss"}.mkString("\n"))
    val syms = defCache.get(d) match {
      case Some(ss) => ss.foreach(_.addCtx(ctx)); ss
      case None => registerDef(d, Nil)(ctx)
    }
    defCache(d) = syms
    syms
  }

  private def registerDef(d: Def, extraDeps: List[Sym[_]])(ctx: SrcCtx): List[Sym[Any]] = {
    val bounds = d.binds
    val tunnels = d.tunnels
    val dfreqs = d.freqs.groupBy(_._1).mapValues(_.map(_._2).sum)
    val freqs = d.inputs.map { in => dfreqs.getOrElse(in, 1.0f) } ++ extraDeps.distinct.map { d => 1.0f }

    val inputs = d.inputs
    val outputs = d.outputTypes.map{tp => __sym(tp) }

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

  def stageDefEffectful(d: Def, u: Effects)(ctx: SrcCtx): List[Sym[_]] = {
    val atomicEffects = propagateWrites(u)

    log(c"Staging $d, effects = $u")
    log(c"  mutable inputs = ${mutableInputs(d)}")
    log(c"  actual writes = ${atomicEffects.writes}")

    val effects = atomicEffects andAlso Read(mutableInputs(d))
    log(c"  full effects = $effects")

    if (effects == Pure) registerDefWithCSE(d)(ctx)
    else if (effects == Cold) registerDef(d, Nil)(ctx)    // Don't add "Cold" effects to context list, but don't CSE
    else {
      checkContext()
      val deps = effectDependencies(effects)

      def stageEffects(): List[Sym[_]] = {
        val ss = registerDef(d, deps)(ctx)
        ss.foreach { s =>
          effectsOf(s) = effectsOf(s) andAlso effects
          depsOf(s) = depsOf(s) ++ deps
          context +:= s // prepend (O(1))
        }
        ss
      }
      if (effects.isIdempotent) {
        // CSE statements which are idempotent and have identical effect summaries (e.g. repeated reads w/o writes)
        val symsWithSameDef = defCache.getOrElse(d, Nil) intersect context
        val symsWithSameEffects = symsWithSameDef.filter { case Effectful(u2, es) => u2 == effects && es == deps }

        if (symsWithSameEffects.isEmpty) stageEffects()
        else {
          symsWithSameEffects.foreach(_.addCtx(ctx))
          symsWithSameEffects
        }
      }
      else {
        val z = stageEffects()
        // Correctness checks -- cannot have mutable aliases, cannot mutate immutable symbols
        val aliases = mutableAliases(d) diff effects.writes
        val immutables = effects.writes.filterNot(isMutable)

        if (aliases.nonEmpty) new IllegalMutableSharingError(z.head, aliases)(ctx)
        if (immutables.nonEmpty) new IllegalMutationError(z.head, immutables)(ctx)
        z
      }
    }
  }


  private def single[T:Staged](xx: List[Sym[_]]): Sym[T] = xx.head.asInstanceOf[Sym[T]]

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
