package argon.codegen.dotgen

import argon.core.compiler._
import argon.nodes._

trait DotGenFltPt extends DotCodegen {

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FltNeg(x)   => 
    case FltAdd(x,y) => 
    case FltSub(x,y) => 
    case FltMul(x,y) => 
    case FltDiv(x,y) => 
    case FltLt(x,y)  => 
    case FltLeq(x,y) => 
    case FltNeq(x,y) => 
    case FltEql(x,y) => 
    case FltRandom(x) => lhs.tp match {
      case FloatType()  => 
      case DoubleType() => 
    }
    case FltConvert(x) => lhs.tp match {
      case FloatType()  => 
      case DoubleType() => 
    }
    case _ => super.emitNode(lhs, rhs)
  }

}
