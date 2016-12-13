package argon.core
import argon.utils.escapeConst

trait Staging extends Statements {
  def fresh[T:Staged]: Sym[T] = single[T](registerDef(NoOp[T](), Nil)(here))
  def const[T:Staged](c: Any)(implicit ctx: SrcCtx): Const[T] = {
    log(c"Making constant ${stg[T]} from ${escapeConst(c)} : ${c.getClass}")
    single[T](registerDef(NoOp[T](), Nil, __const(c))(here)).asInstanceOf[Const[T]]
  }
  def param[T:Staged](c: Any)(implicit ctx: SrcCtx): Param[T] = single[T](registerDef(NoOp[T](), Nil, __param(c))(ctx)).asInstanceOf[Param[T]]
  def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.staged.wrap(const[B](x)(l.staged,ctx))

  def stageDef(d: Def)(ctx: SrcCtx): List[Sym[_]]                   = stageDefPure(d)(ctx)
  def stageDefPure(d: Def)(ctx: SrcCtx): List[Sym[_]]               = stageDefEffectful(d, Pure)(ctx)
  def stageDefWrite(ss: Sym[_]*)(d: Def)(ctx: SrcCtx): List[Sym[_]] = stageDefEffectful(d, Write(ss.toSet))(ctx)
  def stageDefSimple(d: Def)(ctx: SrcCtx): List[Sym[_]]             = stageDefEffectful(d, Simple)(ctx)
  def stageDefMutable(d: Def)(ctx: SrcCtx): List[Sym[_]]            = stageDefEffectful(d, Mutable)(ctx)

  def stage[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                      = single[T](stageDef(op)(ctx))
  def stagePure[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stageDefPure(op)(ctx))
  def stageWrite[T:Staged](ss: Sym[_]*)(op: Op[T])(ctx: SrcCtx): Sym[T]    = single[T](stageDefWrite(ss:_*)(op)(ctx))
  def stageSimple[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stageDefSimple(op)(ctx))
  def stageMutable[T:Staged](op: Op[T])(ctx: SrcCtx): Sym[T]               = single[T](stageDefMutable(op)(ctx))
  def stageEffectful[T:Staged](op: Op[T], u: Effects)(ctx: SrcCtx): Sym[T] = single[T](stageDefEffectful(op, u)(ctx))

  private def registerDefWithCSE(d: Def)(ctx: SrcCtx): List[Sym[_]] = {
    // log(c"Checking defCache for $d")
    // log(c"Def cache: " + defCache.map{case (d,ss) => c"$d -> $ss"}.mkString("\n"))
    val syms = defCache.get(d) match {
      case Some(ss) => ss.map(_.withCtx(ctx))
      case None => registerDef(d, Nil)(ctx)
    }
    defCache(d) = syms
    syms
  }

  private def registerDef(d: Def, extraDeps: List[Sym[_]], symbol: Staged[_] => Sym[Any] = __sym)(ctx: SrcCtx): List[Sym[Any]] = {
    val bounds = d.binds
    val dfreqs = d.freqs.groupBy(_._1).mapValues(_.map(_._2).sum)
    val freqs = d.inputs.map { in => dfreqs.getOrElse(in, 1.0f) } ++ extraDeps.distinct.map { d => 1.0f }

    val inputs = d.inputs
    val outputs = d.outputTypes.map{tp => symbol(tp) }

    addNode(inputs, outputs, bounds, freqs, d)

    log(c"Staging node $d")
    log(c"  outputs = $outputs")
    log(c"  binds = $bounds")
    log(c"  freqs = ${d.inputs}")

    outputs.map(_.setCtx(ctx))
  }



  def stageDefEffectful(d: Def, u: Effects)(ctx: SrcCtx): List[Sym[_]] = {
    log(c"Staging $d, effects = $u")

    val effects = u andAlso Read(mutableInputs(d))

    if (effects == Pure) registerDefWithCSE(d)(ctx)
    else {
      checkContext()
      val deps = effectDependencies(effects)

      def stageEffects(canCSE: Boolean): List[Sym[_]] = {
        val ss = if (canCSE) registerDefWithCSE(d)(ctx) else registerDef(d, deps)(ctx)
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

        if (symsWithSameEffects.isEmpty) stageEffects(canCSE = true)
        else symsWithSameEffects.map(_.withCtx(ctx))
      }
      else {
        val z = stageEffects(canCSE = false)
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
}
