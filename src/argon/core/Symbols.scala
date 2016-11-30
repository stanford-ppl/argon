package argon.core

import argon.graphs.EdgeLike

trait Symbols extends Metadata with Base with Reporting { self: Statements =>

  abstract class Sym { self =>
    type LibType
    def tp:Typ[_]

    private var value:Option[LibType] = None
    private var const:Boolean = false
    final def asConst(v:LibType):self.type = { value = Some(v); const = true; this }
    final def asParam(v:LibType):self.type = { value = Some(v); const = false; this }
    final def isConst = value.isDefined && const
    final def isParam = value.isDefined
    final def getValue = value
    private[Symbols] var __id: Int = 0
    final def id: Int = __id

    final def withCtx(ctx: SrcCtx): Sym = { ctxsOf(this) = ctx +: ctxsOf(this); this }
    final def setCtx(ctx: SrcCtx): Sym = { ctxsOf(this) = List(ctx); this }
    final override def hashCode(): Int = id

    final override def equals(x: Any) = x match {
      case that: Sym =>
        if (this.const && that.const) this.value == that.value && this.tp == that.tp
        else this.id == that.id
      case _ => false
    }
  }

  implicit object SymIsEdgeLike extends EdgeLike[Int, Sym] {
    def setId(s: Sym, id: Int): Unit = { s.__id = id }
    def getId(s :Sym): Int = s.__id
  }


  case class SrcCtxs(pos: List[SrcCtx]) extends Metadata[SrcCtxs] { def mirror(f:Tx) = this }
  object ctxsOf {
    def apply(x: Sym): List[SrcCtx] = metadata[SrcCtxs](x).map(_.pos).getOrElse(Nil)
    def update(x: Sym, ctx: List[SrcCtx]) = metadata.add(x, SrcCtxs(ctx))
  }
  def mpos(pos: List[SrcCtx]) = pos.head
  def mpos(s: Sym) = ctxsOf(s).head


  /** Compiler debugging **/
  override def readable(x: Any): String = x match {
    case s: Sym => s"x${s.id}"
    case s: SrcCtxs => mpos(s.pos).toString
    case t: Typ[_] => readable(t.stagedClass)
    case _ => super.readable(x)
  }

}
