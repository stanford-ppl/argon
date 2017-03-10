package argon.codegen.dotgen

import argon.ops.MixedNumericExp

trait DotGenMixedNumeric extends DotCodegen with DotGenFixPt with DotGenFltPt {
  val IR: MixedNumericExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case FixPtToFltPt(x) => lhs.tp match {
      case DoubleType() => 
      case FloatType()  => 
    }
    case FltPtToFixPt(x) => lhs.tp match {
      case IntType()  => 
      case LongType() => 
    }
    case _ => super.emitNode(lhs, rhs)
  }
}
