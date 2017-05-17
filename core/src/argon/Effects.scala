package argon

import argon.core.CompilerFacing
import forge._

case class AntiDeps(syms: Seq[Exp[_]]) extends Metadata[AntiDeps] with CompilerFacing {
  def mirror(f: Tx) = this
  override val ignoreOnTransform = true // Mirroring of symbol already takes care of identifying anti-dependencies
  override def toStringCompiler = c"$syms"
}

case class Effects(
  cold:    Boolean = false,           // Should not be code motioned or CSEd
  simple:  Boolean = false,           // Requires ordering with respect to other simple effects
  global:  Boolean = false,           // Modifies execution of entire program (e.g. exceptions, exiting)
  mutable: Boolean = false,           // Allocates a mutable structure
  throws:  Boolean = false,           // May throw exceptions (speculative execution may be unsafe)
  reads:   Set[Sym[_]] = Set.empty,   // Reads given mutable symbols
  writes:  Set[Sym[_]] = Set.empty    // Writes given mutable symbols
) extends Metadata[Effects] with CompilerFacing {
  def mirror(f: Tx) = this
  override val ignoreOnTransform = true // Mirroring of symbol already takes care of summarizing effects

  private def combine(that: Effects, m1: Boolean, m2: Boolean) = Effects(
    cold    = this.cold || that.cold,
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
  def isIdempotent = !cold && !simple && !global && !mutable && writes.isEmpty

  def onlyThrows = this == Throws
  def mayWrite(ss: Set[Sym[_]]) = global || ss.exists { s => writes contains s }
  def mayRead(ss: Set[Sym[_]]) = global || ss.exists { s => reads contains s }

  override def toStringCompiler = {
    if (this == Pure) "Pure"
    else if (this == Cold) "Cold"
    else if (this == Mutable) "Mutable"
    else if (this == Simple)  "Simple"
    else if (this == Global)  "Global"
    else if (this == Throws)  "Throws"
    else {
      "(" +
        ((if (cold) List(c"cold=true") else Nil) ++
          (if (simple) List(c"simple=true") else Nil) ++
          (if (global) List(c"global=true") else Nil) ++
          (if (mutable) List("mutable") else Nil) ++
          (if (throws) List("throws") else Nil) ++
          (if (reads.nonEmpty) List(c"""reads={${reads.map(x => c"$x").mkString(",")}}""") else Nil) ++
          (if (writes.nonEmpty) List(c"""writes={${writes.map(x => c"$x").mkString(",")}}""") else Nil)).mkString(", ") + ")"
    }
  }
}
