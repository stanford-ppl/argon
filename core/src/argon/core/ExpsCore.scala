package argon.core

import org.virtualized.EmptyContext
import argon._
import forge._

trait ExpsCore { self: ArgonCore =>

  case class SymbolContext(pos: List[SrcCtx]) extends Metadata[SymbolContext] with CompilerFacing {
    def mirror(f:Tx) = this
    override def toStringCompiler = pos.head.toString()
  }
  private object ctxsOf {
    @stateful def apply(x: Exp[_]): List[SrcCtx] = metadata[SymbolContext](x).map(_.pos).getOrElse(Nil)
    @stateful def update(x: Exp[_], ctx: List[SrcCtx]) = metadata.add(x, SymbolContext(ctx))
  }

  implicit class ExpContextOps(x: Exp[_]) {
    def ctx: SrcCtx = ctxsOf(x).headOption.getOrElse(EmptyContext)
    def ctxOrElse(els: SrcCtx) = ctxsOf(x).headOption.getOrElse(els)
    def addCtx(ctx: SrcCtx) { if (ctx != EmptyContext || ctxsOf(x).isEmpty) ctxsOf(x) = ctxsOf(x) :+ ctx }
    def setCtx(ctx: SrcCtx) { if (ctx != EmptyContext || ctxsOf(x).isEmpty) ctxsOf(x) = List(ctx) }
  }

  case class CtxName(name: String) extends Metadata[CtxName] { def mirror(f:Tx) = this }

  object nameOf {
    @stateful def apply(x: Exp[_]): Option[String] = metadata[CtxName](x).map(_.name)
    @stateful def update(x: Exp[_], name: String) = metadata.add(x, CtxName(name))
  }
}
