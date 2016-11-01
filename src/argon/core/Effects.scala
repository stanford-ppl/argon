package argon.core

import scala.collection.mutable

trait Effects extends Symbols { this: Statements =>
  var context: List[Sym] = _
  final def checkContext(): Unit = if (context == null) throw new UninitializedEffectContextException()

  case class Dependencies(deps: List[Sym]) extends Metadata[Dependencies] {
    def mirror(f:Tx) = Dependencies(f(deps))
  }
  object depsOf {
    def apply(x: Sym) = metadata[Dependencies](x).map(_.deps).getOrElse(Nil)
    def update(x: Sym, deps: List[Sym]) = metadata.add(x, Dependencies(deps))
  }

  case class Effects(
    simple:  Boolean = false,
    global:  Boolean = false,
    mutable: Boolean = false,
    reads:   Set[Sym] = Set.empty,
    writes:  Set[Sym] = Set.empty
  ) extends Metadata[Effects] {
    def mirror(f:Tx) = this.copy(reads = f(reads), writes = f(writes))

    private def combine(that: Effects, m1: Boolean, m2:Boolean) = Effects(
      simple  = this.simple || that.simple,
      global  = this.global || that.global,
      mutable = (m1 && this.mutable) || (m2 && that.mutable),
      reads   = this.reads union that.reads,
      writes  = this.writes union that.writes
    )
    def orElse(that: Effects) = this.combine(that, m1=false,m2=false)
    def andAlso(that: Effects) = this.combine(that, m1=true,m2=true)
    def andThen(that: Effects) = this.combine(that, m1=false,m2=true)
    def star = this.copy(mutable = false) // Pure orElse this

    def isPure = !simple && !global && !mutable && reads.isEmpty && writes.isEmpty
    def isMutable = mutable
    def isIdempotent = !simple && !global && !mutable && writes.isEmpty
    def mayWrite(ss: Set[Sym]) = global || ss.exists{s => writes contains s}
    def mayRead(ss: Set[Sym]) = global || ss.exists{s => reads contains s}
  }
  val Pure    = Effects()
  val Simple  = Effects(simple = true)
  val Global  = Effects(global = true)
  val Mutable = Effects(mutable = true)
  def Read(s: Sym) = Effects(reads = Set(s))
  def Read(ss: Set[Sym]) = Effects(reads = ss)
  def Write(s: Sym) = Effects(writes = Set(s))
  def Write(ss: Set[Sym]) = Effects(writes = ss)

  object effectsOf {
    def apply(s: Sym) = metadata[Effects](s).getOrElse(Pure)
    def update(s: Sym, e: Effects) = metadata.add(s, e)
  }
  object Effectful {
    def unapply(x: Sym): Option[(Effects,List[Sym])] = {
      val deps = depsOf(x)
      val effects = effectsOf(x)
      if (effects.isPure && deps.isEmpty) None else Some((effects,deps))
    }
  }

  final def isMutable(s: Sym): Boolean = metadata[Effects](s).exists(_.mutable)

  final def effectDependencies(effects: Effects): List[Sym] = if (effects.global) context else {
    val read = effects.reads
    val write = effects.writes
    val accesses = read ++ write  // Cannot read/write prior to allocation

    var unwrittenAccesses = accesses
    // Find most recent write to each accessed memory
    var hazards = mutable.ListBuffer[Sym]()

    val iter = context.iterator
    while (iter.hasNext) {
      iter.next match { case e@Effectful(u,_) =>
        if (u.mayRead(write)) hazards += e    // WAR hazards
        if (unwrittenAccesses.isEmpty) {
          val (written, unwritten) = unwrittenAccesses.partition(u.writes.contains)
          unwrittenAccesses = unwritten
          hazards ++= written                 // *AW hazards
        }
        if (accesses contains e) hazards += e // "AAA" hazards (access after allocate)
      }
    }
    val simpleDep  = if (effects.simple) context.find{case Effectful(u,_) => u.simple } else None
    val globalDep  = context.find{case Effectful(u,_) => u.global }

    hazards.result ++ simpleDep ++ globalDep
  }

  /** Compiler debugging **/
  override def readable(a: Any) = a match {
    case d: Dependencies => c"${d.deps}"
    case e: Effects =>
      if (e == Pure) "Pure"
      else if (e == Mutable) "Mutable"
      else if (e == Simple)  "Simple"
      else if (e == Global)  "Global"
      else {
        "(" +
           ((if (e.simple) List(c"simple=${e.simple}") else Nil) ++
            (if (e.global) List(c"global=${e.global}") else Nil) ++
            (if (e.mutable)  List("mutable") else Nil) ++
            (if (e.reads.nonEmpty) List(c"reads=${e.reads}") else Nil) ++
            (if (e.writes.nonEmpty) List(c"writes=${e.writes}") else Nil)).mkString(", ") + ")"
      }
    case _ => super.readable(a)
  }

}
