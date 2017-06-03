package argon.core

import ops._
import forge._

case class AntiDeps(syms: Seq[Exp[_]]) extends Metadata[AntiDeps] with CompilerFacing {
  def mirror(f: Tx) = this
  override val ignoreOnTransform = true // Mirroring of symbol already takes care of identifying anti-dependencies
  override def toStringCompiler = c"$syms"
}

case class Effects(
  unique:  Boolean = false,           // Should not be CSEd
  sticky:  Boolean = false,           // Should not be code motioned out of blocks
  simple:  Boolean = false,           // Requires ordering with respect to other simple effects
  global:  Boolean = false,           // Modifies execution of entire program (e.g. exceptions, exiting)
  mutable: Boolean = false,           // Allocates a mutable structure
  throws:  Boolean = false,           // May throw exceptions (speculative execution may be unsafe)
  reads:   Set[Sym[_]] = Set.empty,   // Reads given mutable symbols
  writes:  Set[Sym[_]] = Set.empty    // Writes given mutable symbols
) extends Metadata[Effects] {
  def mirror(f: Tx) = Effects(unique, sticky, simple, global, mutable, throws, f.txSyms(reads), f.txSyms(writes))
  override val ignoreOnTransform = true // Mirroring already takes care of effects

  private def combine(that: Effects, m1: Boolean, m2: Boolean) = Effects(
    unique  = this.unique || that.unique,
    sticky  = this.sticky || that.sticky,
    simple  = this.simple || that.simple,
    global  = this.global || that.global,
    mutable = (m1 && this.mutable) || (m2 && that.mutable),
    throws  = this.throws || that.throws,
    reads   = this.reads union that.reads,
    writes  = this.writes union that.writes
  )
  def orElse(that: Effects) = this.combine(that, m1 = false, m2 = false)
  def andAlso(that: Effects) = this.combine(that, m1 = true, m2 = true)
  def andThen(that: Effects) = this.combine(that, m1 = false, m2 = true)
  def star = this.copy(mutable = false) // Pure orElse this

  def isPure = this == Pure
  def isMutable = mutable
  def isIdempotent = !simple && !global && !mutable && writes.isEmpty
  def mayCSE = isIdempotent && !unique

  def onlyThrows = this == Throws
  def mayWrite(ss: Set[Sym[_]]) = global || ss.exists { s => writes contains s }
  def mayRead(ss: Set[Sym[_]]) = global || ss.exists { s => reads contains s }
}
