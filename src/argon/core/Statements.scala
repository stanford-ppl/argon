package argon.core

import argon.Config

import scala.collection.mutable

trait Statements extends Definitions with ArgonExceptions { this: Staging =>
  // -- State
  val defCache = new mutable.HashMap[Def, List[Sym[_]]]
  protected val shallowAliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]
  protected val deepAliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]
  protected val aliasCache = new mutable.HashMap[Sym[_], Set[Sym[_]]]

  // --- Statements
  case class Stm(lhs: List[Sym[_]], rhs: Def)

  // TODO: LMS Naming convention was weird here. What do these even stand for?
  object TP {
    def unapply[A](x: Any): Option[(Sym[Any],Op[Any])] = x match {
      case Stm(List(lhs), rhs: Op[_]) => Some((lhs.asInstanceOf[Sym[Any]], rhs.asInstanceOf[Op[Any]]))
      case _ => None
    }
  }
  object TTP {
    def unapply(x: Any): Option[(List[Sym[_]],Def)] = x match {
      case Stm(_, rhs: Op[_]) => None
      case stm: Stm => Some((stm.lhs,stm.rhs))
      case _ => None
    }
  }


  // --- Helper functions
  // Getting statement returns Option to account for Bounds, but this is known to be a Sym
  def stmOf(sym: Sym[_]): Stm = stmFromSymId(sym.id).get

  private[argon] def stmFromNodeId(id: Int): Option[Stm] = {
    val x = triple(id)
    x._1.head match {
      case _:Sym[_] =>
        val lhs = x._1.toList.map(_.asInstanceOf[Sym[_]])
        val rhs = x._2.asInstanceOf[Def]
        Some(Stm(lhs, rhs))

      case _:Bound[_] => None
    }
  }
  private[argon] def stmFromSymId(id: Int): Option[Stm] = {
    val node = producerOf(id)
    stmFromNodeId(node)
  }
  private[argon] def symFromSymId(id: Int): Symbol[_] = edgeOf(id).asInstanceOf[Symbol[_]]
  private[argon] def defFromNodeId(id: Int): Def = nodeOf(id).asInstanceOf[Def]
  private[argon] def defFromSymId(id: Int): Option[Def] = stmFromSymId(id).map(_.rhs)

  // --- Symbol aliasing
  private def noPrims(x: Set[Symbol[_]]): Set[Sym[_]] = x.collect{case s: Sym[_] if !s.tp.isPrimitive => s}

  def shallowAliases(x: Any): Set[Sym[_]] = {
    noPrims(aliasSyms(x)).flatMap { case s@Def(d) => shallowAliasCache.getOrElseUpdate(s, shallowAliases(d)) + s } ++
      noPrims(containSyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) }
  }
  def deepAliases(x: Any): Set[Sym[_]] = {
    noPrims(aliasSyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) } ++
      noPrims(copySyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) } ++
      noPrims(extractSyms(x)).flatMap { case s@Def(d) => aliasCache.getOrElseUpdate(s, allAliases(d)) + s } ++
      noPrims(containSyms(x)).flatMap { case s@Def(d) => deepAliasCache.getOrElseUpdate(s, deepAliases(d)) }
  }
  final def allAliases(x: Any): Set[Sym[_]] = {
    shallowAliases(x) ++ deepAliases(x)
  }
  final def mutableAliases(x: Any): Set[Sym[_]] = allAliases(x).filter(isMutable)
  final def mutableInputs(d: Def): Set[Sym[_]] = {
    val bounds = d.binds
    val actuallyReadSyms = d.reads diff bounds
    mutableAliases(actuallyReadSyms) filterNot (bounds contains _)
  }

  /** Used to allow nested ("atomic") writes, which are reflected on the top mutable object rather than intermediates
    * e.g.
    *   val b = Array(1, 2, 3)
    *   val a = MutableStruct(b, ...)
    *   a.b(0) = 1
    * Should become a write on (the mutable symbol) a instead of the immutable symbol resulting from a.b
    *
    * TODO: Any reason for this to be Sym[_] => Seq[Sym[_]] ?
    */
  def recurseAtomicLookup(s: Exp[_]): Exp[_] = s
  final def extractAtomicWrite(s: Sym[_]): Sym[_] = onlySyms(recurseAtomicLookup(s)).headOption.getOrElse(s)

  final def propagateWrites(effects: Effects): Effects = if (!Config.allowAtomicWrites) effects else {
    val writes = effects.writes.map{s => extractAtomicWrite(s) }
    effects.copy(writes = writes)
  }
}
