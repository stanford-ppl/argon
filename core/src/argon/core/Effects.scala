package argon.core

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
  func:    Boolean = false,           // Represents a function declaration
  reads:   Set[Sym[_]] = Set.empty,   // Reads given mutable symbols
  writes:  Set[Sym[_]] = Set.empty    // Writes given mutable symbols
) extends Metadata[Effects] with CompilerFacing {
  def mirror(f: Tx) = Effects(unique, sticky, simple, global, mutable, throws, func, f.txSyms(reads), f.txSyms(writes))
  override val ignoreOnTransform = true // Mirroring already takes care of effects

  private def combine(that: Effects, m1: Boolean, m2: Boolean) = Effects(
    unique  = this.unique || that.unique,
    sticky  = this.sticky || that.sticky,
    simple  = this.simple || that.simple,
    global  = this.global || that.global,
    mutable = (m1 && this.mutable) || (m2 && that.mutable),
    throws  = this.throws || that.throws,
    func    = this.func || that.func,
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
  def isFunction = func
  def mayCSE = isIdempotent && !unique

  def onlyThrows = this == Throws
  def mayWrite(ss: Set[Sym[_]]) = global || ss.exists { s => writes contains s }
  def mayRead(ss: Set[Sym[_]]) = global || ss.exists { s => reads contains s }

  override def toStringCompiler = {
    if (this == Pure) "Pure"
    else if (this == Unique)  "Unique"
    else if (this == Sticky)  "Sticky"
    else if (this == Mutable) "Mutable"
    else if (this == Simple)  "Simple"
    else if (this == Global)  "Global"
    else if (this == Throws)  "Throws"
    else {
      "(" +
        ((if (this.unique) List(c"unique=${this.unique}") else Nil) ++
          (if (this.sticky) List(c"sticky=${this.sticky}") else Nil) ++
          (if (this.simple) List(c"simple=${this.simple}") else Nil) ++
          (if (this.global) List(c"global=${this.global}") else Nil) ++
          (if (this.mutable)  List("mutable") else Nil) ++
          (if (this.throws) List("throws") else Nil) ++
          (if (this.func) List("function") else Nil) ++
          (if (this.reads.nonEmpty) List(c"""reads={${this.reads.map(x=> c"$x").mkString(",")}}""") else Nil) ++
          (if (this.writes.nonEmpty) List(c"""writes={${this.writes.map(x=> c"$x").mkString(",")}}""") else Nil)).mkString(", ") + ")"
    }
  }
}
