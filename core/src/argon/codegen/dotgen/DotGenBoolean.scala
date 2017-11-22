package argon.codegen.dotgen

import argon.core._
import argon.nodes._

trait DotGenBoolean extends DotCodegen {

  override def attr(n: Exp[_]) = n match {
    case Def(_:Not)  => super.attr(n).shape(circle).label("~")
    case Def(_:And)  => super.attr(n).shape(circle).label("&")
    case Def(_:Or)   => super.attr(n).shape(circle).label("|")
    case Def(_:XOr)  => super.attr(n).shape(circle).label("!=")
    case Def(_:XNor) => super.attr(n).shape(circle).label("==")
    case Def(_:RandomBoolean) => super.attr(n).shape(diamond).label("rnd")
    case _ => super.attr(n)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Not(x)       => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs);}
    case And(x,y)     => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case Or(x,y)      => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case XOr(x,y)     => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case XNor(x,y)    => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); emitEdge(y, lhs)}
    case RandomBoolean(x) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x, lhs); }
    case _ => super.emitNode(lhs, rhs)
  }
}
