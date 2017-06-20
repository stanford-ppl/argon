package argon.lang

import argon.core._
import argon.nodes._
import forge._

object IfThenElseOps {
  /** Constructors **/
  @internal def ifThenElse[T:Type](cond: Exp[MBoolean], thenp: () => Exp[T], elsep: () => Exp[T]): Exp[T] = cond match {
    case Const(true) if state.context != null => thenp() // Inlining is not valid if there is no outer context
    case Const(false) if state.context != null => elsep()
    case Op(Not(x)) => ifThenElse(x, elsep, thenp)
    case _ =>
      val thenBlk = stageColdBlock(thenp())
      val elseBlk = stageColdBlock(elsep())
      val effects = thenBlk.effects orElse elseBlk.effects
      stageEffectful(IfThenElse(cond, thenBlk, elseBlk), effects)(ctx)
  }
}
