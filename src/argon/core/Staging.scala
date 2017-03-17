package argon.core
import argon.utils.escapeConst

trait Staging extends Statements {
  def fresh[T:BStaged]: Bound[T] = {
    val bnd = __bound[T]
    addBound(bnd)
    bnd
  }
  def constant[T:BStaged](c: Any)(implicit ctx: SrcCtx): Const[T] = {
    val cc = __const[T](c)
    log(c"Making constant ${btyp[T]} from ${escapeConst(c)} : ${c.getClass}")
    registerInput(cc)
    cc.setCtx(ctx)
    cc
  }
  def parameter[T:BStaged](c: Any)(implicit ctx: SrcCtx): Param[T] = {
    val p = __param[T](c)
    log(c"Making parameter ${btyp[T]} from ${escapeConst(p)} : ${c.getClass}")
    registerInput(p)
    p.setCtx(ctx)
    p
  }

  def __lift[A,B](x: A)(implicit ctx: SrcCtx, l: Lift[A,B]): B = l.FStaged.wrapped(constant[B](x)(l.FStaged,ctx))

  def stagedef(d: Def)(ctx: SrcCtx): List[Sym[_]]                   = stagedefPure(d)(ctx)
  def stagedefPure(d: Def)(ctx: SrcCtx): List[Sym[_]]               = stagedefEffectful(d, Pure)(ctx)
  def stagedefCold(d: Def)(ctx: SrcCtx): List[Sym[_]]               = stagedefEffectful(d, Cold)(ctx)
  def stagedefWrite(ss: Exp[_]*)(d: Def)(ctx: SrcCtx): List[Sym[_]] = stagedefEffectful(d, Write(ss:_*))(ctx)
  def stagedefSimple(d: Def)(ctx: SrcCtx): List[Sym[_]]             = stagedefEffectful(d, Simple)(ctx)
  def stagedefGlobal(d: Def)(ctx: SrcCtx): List[Sym[_]]             = stagedefEffectful(d, Global)(ctx)
  def stagedefMutable(d: Def)(ctx: SrcCtx): List[Sym[_]]            = stagedefEffectful(d, Mutable)(ctx)

  def stage[T](op: Op[T])(ctx: SrcCtx): Sym[T]                      = single[T](stagedef(op)(ctx))(op.bStaged)
  def stagePure[T](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stagedefPure(op)(ctx))(op.bStaged)
  def stageCold[T](op: Op[T])(ctx: SrcCtx): Sym[T]                  = single[T](stagedefCold(op)(ctx))(op.bStaged)
  def stageWrite[T](ss: Exp[_]*)(op: Op[T])(ctx: SrcCtx): Sym[T]    = single[T](stagedefWrite(ss:_*)(op)(ctx))(op.bStaged)
  def stageSimple[T](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stagedefSimple(op)(ctx))(op.bStaged)
  def stageGlobal[T](op: Op[T])(ctx: SrcCtx): Sym[T]                = single[T](stagedefGlobal(op)(ctx))(op.bStaged)
  def stageMutable[T](op: Op[T])(ctx: SrcCtx): Sym[T]               = single[T](stagedefMutable(op)(ctx))(op.bStaged)
  def stageEffectful[T](op: Op[T], u: Effects)(ctx: SrcCtx): Sym[T] = single[T](stagedefEffectful(op, u)(ctx))(op.bStaged)

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

  def stagedefEffectful(d: Def, u: Effects)(ctx: SrcCtx): List[Sym[_]] = {
    val atomicEffects = propagateWrites(u)

    log(c"Staging $d, effects = $u")
    log(c"  mutable inputs = ${mutableInputs(d)}")
    log(c"  actual writes = ${atomicEffects.writes}")

    val effects = atomicEffects andAlso Read(mutableInputs(d))
    log(c"  full effects = $effects")
    log(c"  isIdempotent = ${effects.isIdempotent}")

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


  private def single[T:BStaged](xx: List[Sym[_]]): Sym[T] = xx.head.asInstanceOf[Sym[T]]


  // TODO: where does this actually belong?
  def makeScopeIndex(scope: Iterable[Stm]): OrderCache = buildScopeIndex(scope.map(_.rhs.id))
  def orderedInputs(roots: Iterable[Exp[_]], cache: OrderCache): List[Stm] = {
    scheduleDepsWithIndex(syms(roots).map(_.id), cache).flatMap(stmFromNodeId)
  }

  def schedule(roots: Iterable[Stm], checkAcyclic: Boolean = true)(next: Exp[_] => List[Stm]): List[Stm] = {
    def succ(node: NodeId): Iterable[NodeId] = nodeOutputs(node).map(symFromSymId).flatMap(next).map(_.rhs.id)

    val start = roots.map(_.rhs.id)

    val ids = if (checkAcyclic) {
      val xx = sccs(start){node => succ(node) }
      checkIfAcyclic(roots, xx)
      xx.flatten.reverse
    }
    else {
      dfs(start){node => succ(node) }
    }
    ids.flatMap(stmFromNodeId)
  }


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
