package argon.analysis

import argon.core._
import argon.traversal._

case class ParamFinalizer(var IR: State) extends Traversal {
  override val name = "Param Finalizer"
  override val recurse = Always

  override protected def visit(lhs: Sym[_], rhs: Op[_]): Unit = {
    dbgs(s"$lhs = $rhs")
    rhs.expInputs.foreach{
      case p: Param[_] =>
        dbgs(c"Found parameter $p")
        p.makeFinal()
      case _ => // Do nothing
    }
  }

}
