package argon.codegen.dotgen

import argon.core._
import argon.nodes._

trait DotGenFixPt extends DotCodegen {
  override def attr(n:Exp[_]) = n match {
    case lhs: Sym[_] => lhs match {
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
      case _ => super.attr(n)
    }
    case _ => super.attr(n)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FixInv(x)   => 
    case FixNeg(x)   => 
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
    case FixRandom(x) => lhs.tp match {
      case IntType()  => 
      case LongType() => 
    }
    case FixConvert(x) => if (config.dotDetail > 0) {emitVert(lhs); emitEdge(x,lhs);}
    case _ => super.emitNode(lhs, rhs)
  }
}
