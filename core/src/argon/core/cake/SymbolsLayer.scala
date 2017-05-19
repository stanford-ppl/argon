package argon.core
package cake

import forge._
import org.virtualized.EmptyContext

trait SymbolsLayer { self: ArgonCore =>

  case class SymbolContext(pos: List[SrcCtx]) extends Metadata[SymbolContext] with CompilerFacing {
    def mirror(f:Tx) = this
    override def toStringCompiler = pos.head.toString()
  }
  private object ctxsOf {
    @stateful def apply(x: Exp[_]): List[SrcCtx] = metadata[SymbolContext](x).map(_.pos).getOrElse(Nil)
    @stateful def update(x: Exp[_], ctx: List[SrcCtx]) = metadata.add(x, SymbolContext(ctx))
  }

  implicit class ExpContextOps(x: Exp[_]) {
    @stateful def ctx: SrcCtx = ctxsOf(x).headOption.getOrElse(EmptyContext)
    @stateful def ctxOrElse(els: SrcCtx) = ctxsOf(x).headOption.getOrElse(els)
    @stateful def addCtx(ctx: SrcCtx) { if (ctx != EmptyContext || ctxsOf(x).isEmpty) ctxsOf(x) = ctxsOf(x) :+ ctx }
    @stateful def setCtx(ctx: SrcCtx) { if (ctx != EmptyContext || ctxsOf(x).isEmpty) ctxsOf(x) = List(ctx) }
  }
}
