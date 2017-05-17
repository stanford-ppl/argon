package argon.codegen.dotgen

import argon._
import argon.nodes._

trait DotGenBoolean extends DotCodegen {

  override def attr(n:Exp[_]) = n match {
    case lhs: Sym[_] => lhs match {
      case Def(Not(x)       ) => super.attr(n).label("~")
      case Def(And(x,y)     ) => super.attr(n).label("&")
      case Def(Or(x,y)      ) => super.attr(n).label("|")
      case Def(XOr(x,y)     ) => super.attr(n).label("^")
      case Def(XNor(x,y)    ) => super.attr(n).label("xnor")
      case Def(RandomBoolean(x)) => super.attr(n).label("rnd")
      case _ => super.attr(n)
    }
    case _ => super.attr(n)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)       => if (Config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs);}
    case And(x,y)     => if (Config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case Or(x,y)      => if (Config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case XOr(x,y)     => if (Config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case XNor(x,y)    => if (Config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case RandomBoolean(x) => if (Config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); }
    case _ => super.emitNode(lhs, rhs)
  }
}
