package argon.codegen.dotgen

import argon.ops.StringCastExp

trait DotGenStringCast extends DotCodegen {
  val IR: StringCastExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case StringToFltPt(x) => lhs.tp match {
      case DoubleType() => 
      case FloatType()  => 
    }

    case StringToFixPt(x) => lhs.tp match {
      case IntType()  => 
      case LongType() => 
    }

    case StringToBool(x) => 

    case _ => super.emitNode(lhs, rhs)
  }
}
