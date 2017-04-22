package argon.core

trait Effects extends Symbols { this: Staging =>
  var context: List[Sym[_]] = _
  final def checkContext(): Unit = if (context == null) throw new UninitializedEffectContextException()

  case class Dependencies(deps: Seq[Exp[_]]) extends Metadata[Dependencies] {
    def mirror(f:Tx) = Dependencies(f.tx(deps))
    override val ignoreOnTransform = true // Mirroring already takes care of identifying dependencies
  }
  object depsOf {
    def apply(x: Exp[_]) = metadata[Dependencies](x).map(_.deps).getOrElse(Nil)
    def update(x: Exp[_], deps: Seq[Exp[_]]) = metadata.add(x, Dependencies(deps))
  }

  case class Effects(
    cold:    Boolean = false,           // Should not be code motioned or CSEd
    simple:  Boolean = false,           // Requires ordering with respect to other simple effects
    global:  Boolean = false,           // Modifies execution of entire program (e.g. exceptions, exiting)
    mutable: Boolean = false,           // Allocates a mutable structure
    throws:  Boolean = false,           // May throw exceptions (speculative execution may be unsafe)
    reads:   Set[Sym[_]] = Set.empty,   // Reads given mutable symbols
    writes:  Set[Sym[_]] = Set.empty    // Writes given mutable symbols
  ) extends Metadata[Effects] {
    def mirror(f: Tx) = Effects(cold, simple, global, mutable, throws, f.txSyms(reads), f.txSyms(writes))
    override val ignoreOnTransform = true // Mirroring already takes care of effects

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
  }
  val Pure    = Effects()
  val Cold    = Effects(cold = true)
  val Simple  = Effects(simple = true)
  val Global  = Effects(global = true)
  val Mutable = Effects(mutable = true)
  val Throws  = Effects(throws = true)
  def Read(xs: Exp[_]*)  = Effects(reads =  syms(xs).toSet)
  def Write(xs: Exp[_]*) = Effects(writes = syms(xs).toSet)
  def Read(xs: Set[Sym[_]]) = Effects(reads = xs)
  def Write(xs: Set[Sym[_]]) = Effects(writes = xs)

  object effectsOf {
    def apply(s: Exp[_]) = metadata[Effects](s).getOrElse(Pure)
    def update(s: Sym[_], e: Effects) = metadata.add(s, e)
  }
  object Effectful {
    def unapply(x: Sym[_]): Option[(Effects,Seq[Exp[_]])] = {
      val deps = depsOf(x)
      val effects = effectsOf(x)
      if (effects.isPure && deps.isEmpty) None else Some((effects,deps))
    }
  }

  final def isMutable(s: Exp[_]): Boolean = metadata[Effects](s).exists(_.mutable)

  /**
    * Find scheduling dependencies in context
    * WAR - always include reads as scheduling dependencies of writes
    * "AAA" - always include allocation as scheduling dependencies of an access (read or write)
    * RAW/WAW - include the *most recent* write as scheduling dependency of an access ("AAW" - access after write)
    * simple - include the *most recent* previous simple effect as a scheduling dependency of a simple effect
    * global - include ALL global effects as scheduling dependencies of a global effect
    */
  final def effectDependencies(effects: Effects): Seq[Sym[_]] = if (effects.global) context else {
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

    val hazards = context.filter{case e@Effectful(u,_) => isWARHazard(u) || isAAWHazard(u) || (accesses contains e) }
    val simpleDep = if (effects.simple) context.find{case Effectful(u,_) => u.simple } else None // simple
    val globalDep = context.find{case Effectful(u,_) => u.global } // global

    hazards ++ simpleDep ++ globalDep
  }

  /** Compiler debugging **/
  override def readable(a: Any): String = a match {
    case d: Dependencies => c"${d.deps}"
    case e: Effects =>
      if (e == Pure) "Pure"
      else if (e == Cold) "Cold"
      else if (e == Mutable) "Mutable"
      else if (e == Simple)  "Simple"
      else if (e == Global)  "Global"
      else if (e == Throws)  "Throws"
      else {
        "(" +
           ((if (e.cold) List(c"cold=${e.cold}") else Nil) ++
            (if (e.simple) List(c"simple=${e.simple}") else Nil) ++
            (if (e.global) List(c"global=${e.global}") else Nil) ++
            (if (e.mutable)  List("mutable") else Nil) ++
            (if (e.throws) List("throws") else Nil) ++
            (if (e.reads.nonEmpty) List(c"""reads={${e.reads.map(readable).mkString(",")}}""") else Nil) ++
            (if (e.writes.nonEmpty) List(c"""writes={${e.writes.map(readable).mkString(",")}}""") else Nil)).mkString(", ") + ")"
      }
    case _ => super.readable(a)
  }


  override def reset(): Unit = {
    super.reset()
    context = null
  }

}
