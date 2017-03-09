package argon.codegen.dotgen

import argon.ops.FixPtExp

trait DotGenFixPt extends DotCodegen {
  val IR: FixPtExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case FixInv(x)   => 
    case FixNeg(x)   => 
    case FixAdd(x,y) => 
    case FixSub(x,y) => 
    case FixMul(x,y) => 
    case FixDiv(x,y) => 
    case FixAnd(x,y) => 
    case FixOr(x,y)  => 
    case FixLt(x,y)  => 
    case FixLeq(x,y) => 
    case FixNeq(x,y) => 
    case FixEql(x,y) => 
    case FixMod(x,y) => 
    case FixRandom(x) => lhs.tp match {
      case IntType()  => 
      case LongType() => 
    }
    case FixConvert(x) => lhs.tp match {
      case IntType()  => 
      case LongType() => 
      case FixPtType(s,d,f) => 
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
