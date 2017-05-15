package argon.core

import argon._
import forge._

trait EffectsCore { this: ArgonCore =>

  object depsOf {
    @stateful def apply(x: Exp[_]): Seq[Exp[_]] = metadata[AntiDeps](x).map(_.syms).getOrElse(Nil)
    @stateful def update(x: Exp[_], deps: Seq[Exp[_]]): Unit = metadata.add(x, AntiDeps(deps))
  }

  object effectsOf {
    @stateful def apply(s: Exp[_]): Effects = metadata[Effects](s).getOrElse(Pure)
    @stateful def update(s: Sym[_], e: Effects): Unit = metadata.add(s, e)
  }

  object Effectful {
    @stateful def unapply(x: Sym[_]): Option[(Effects,Seq[Exp[_]])] = {
      val deps = depsOf(x)
      val effects = effectsOf(x)
      if (effects.isPure && deps.isEmpty) None else Some((effects,deps))
    }
  }

  final def Read(xs: Exp[_]*)  = Effects(reads = syms(xs).toSet)
  final def Write(xs: Exp[_]*) = Effects(writes = syms(xs).toSet)
  final def Read(xs: Set[Sym[_]]) = Effects(reads = xs)
  final def Write(xs: Set[Sym[_]]) = Effects(writes = xs)

  final def isMutable(s: Exp[_]): Boolean = metadata[Effects](s).exists(_.mutable)

  /**
    * Find scheduling dependencies in context
    * WAR - always include reads as scheduling dependencies of writes
    * "AAA" - always include allocation as scheduling dependencies of an access (read or write)
    * RAW/WAW - include the *most recent* write as scheduling dependency of an access ("AAW" - access after write)
    * simple - include the *most recent* previous simple effect as a scheduling dependency of a simple effect
    * global - include ALL global effects as scheduling dependencies of a global effect
    */
  @stateful final def effectDependencies(effects: Effects): Seq[Sym[_]] = if (effects.global) state.context else {
    val read = effects.reads
    val write = effects.writes
    val accesses = read ++ write  // Cannot read/write prior to allocation

    def isWARHazard(u: Effects) = u.mayRead(write)

    // RAW / WAW
    var unwrittenAccesses = accesses // Reads/writes for which we have not yet found a previous writer
    def isAAWHazard(u: Effects) = {
      if (unwrittenAccesses.nonEmpty) {
        val (written, unwritten) = unwrittenAccesses.partition(u.writes.contains)
        unwrittenAccesses = unwritten
        written.nonEmpty
      }
      else false
    }

    val hazards = state.context.filter{case e@Effectful(u,_) => isWARHazard(u) || isAAWHazard(u) || (accesses contains e) }
    val simpleDep = if (effects.simple) state.context.find{case Effectful(u,_) => u.simple } else None // simple
    val globalDep = state.context.find{case Effectful(u,_) => u.global } // global

    hazards ++ simpleDep ++ globalDep
  }
}
