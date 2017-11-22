package argon.codegen.dotgen

import argon.core._
import argon.nodes._

trait DotGenFltPt extends DotCodegen {

  override def attr(n:Exp[_]) = n match {
    case Def(FltNeg(_))   => super.attr(n).shape(circle).label("-x")
    case Def(FltAdd(_,_)) => super.attr(n).shape(circle).label("+")
    case Def(FltSub(_,_)) => super.attr(n).shape(circle).label("-")
    case Def(FltDiv(_,_)) => super.attr(n).shape(circle).label("/")
    case Def(FltMul(_,_)) => super.attr(n).shape(circle).label("*")
    case Def(FltLt(_,_))  => super.attr(n).shape(circle).label("<")
    case Def(FltLeq(_,_)) => super.attr(n).shape(circle).label("<=")
    case Def(FltNeq(_,_)) => super.attr(n).shape(circle).label("!=")
    case Def(FltEql(_,_)) => super.attr(n).shape(circle).label("==")
    case Def(FltRandom(_)) => super.attr(n).shape(diamond).label("rnd")
    case Def(FltConvert(_)) => super.attr(n).shape(circle).label("conv")
    case Def(FltPtToFixPt(_)) => super.attr(n).shape(circle).label("ft2fx")
    case _ => super.attr(n)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FltNeg(x)   => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs) }
    case FltAdd(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltSub(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltMul(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltDiv(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltLt(x,y)  => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltLeq(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltNeq(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltEql(x,y) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs) }
    case FltRandom(x) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs) }
    case FltConvert(x) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs) }
    case FltPtToFixPt(x) => if (config.dotDetail > 0) { emitVert(lhs); emitEdge(x,lhs) }
    case _ => super.emitNode(lhs, rhs)
  }

}
