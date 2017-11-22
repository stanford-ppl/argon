package argon.codegen.dotgen

import argon.core._
import argon.nodes._

trait DotGenFixPt extends DotCodegen {
  override def attr(n:Exp[_]) = n match {
    case Def(FixInv(_))   => super.attr(n).shape(circle).label("~")
    case Def(FixNeg(_))   => super.attr(n).shape(circle).label("-x")
    case Def(FixAdd(_,_)) => super.attr(n).shape(circle).label("+")
    case Def(FixSub(_,_)) => super.attr(n).shape(circle).label("-")
    case Def(FixDiv(_,_)) => super.attr(n).shape(circle).label("/")
    case Def(FixMul(_,_)) => super.attr(n).shape(circle).label("*")
    case Def(FixAnd(_,_)) => super.attr(n).shape(circle).label("&")
    case Def(FixOr(_,_))  => super.attr(n).shape(circle).label("|")
    case Def(FixXor(_,_)) => super.attr(n).shape(circle).label("^")
    case Def(FixLt(_,_))  => super.attr(n).shape(circle).label("<")
    case Def(FixLeq(_,_)) => super.attr(n).shape(circle).label("<=")
    case Def(FixNeq(_,_)) => super.attr(n).shape(circle).label("!=")
    case Def(FixEql(_,_)) => super.attr(n).shape(circle).label("==")
    case Def(FixMod(_,_)) => super.attr(n).shape(circle).label("%")
    case Def(FixLsh(_,_)) => super.attr(n).shape(circle).label("<<")
    case Def(FixRsh(_,_)) => super.attr(n).shape(circle).label(">>")
    case Def(FixURsh(_,_)) => super.attr(n).shape(circle).label(">>>")
    case Def(FixRandom(_)) => super.attr(n).shape(diamond).label("rnd")
    case Def(FixConvert(_)) => super.attr(n).shape(circle).label("cnv")
    case Def(FixPtToFltPt(_)) => super.attr(n).shape(circle).label("fx2ft")
    case Def(StringToFixPt(_)) => super.attr(n).shape(circle).label("str2fx")
    case _ => super.attr(n)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FixInv(x)   => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs) }
    case FixNeg(x)   => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs) }
    case FixAdd(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixSub(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixMul(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixDiv(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixAnd(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixOr(x,y)  => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixXor(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixLt(x,y)  => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixLeq(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixNeq(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixEql(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixMod(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixLsh(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixRsh(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixURsh(x,y) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs); emitEdge(y,lhs)}
    case FixRandom(x) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs) }
    case FixConvert(x) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs) }
    case FixPtToFltPt(x) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs) }
    case StringToFixPt(x) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs) }
    case _ => super.emitNode(lhs, rhs)
  }
}
