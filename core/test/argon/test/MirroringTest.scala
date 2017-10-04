package argon.test

import argon.core._
import argon.compiler._
import argon.transform.ForwardTransformer
import org.virtualized._

object MirroringTest extends Test { self =>
  import api._
  override def main() = {}

  // Note: All of this internal stuff shouldn't necessarily be visible to the user, just need it for debugging for now
  override def main(args: scala.Array[java.lang.String]) = {
    super.main(args)
    IR.context = Nil
    config.verbosity = 3
    withLog(config.logDir, "MirroringTest.log") {
      val x = Array.empty[Int](1)
      val y = Array.update(x.s, FixPt.int32s(0), FixPt.int32s(16))
      val z = Array.empty[Int](2)
      val tx = new ForwardTransformer { var IR: State = self.IR }

      val y2 = tx.withSubstScope(x.s -> z.s) {
        val y2 = tx.mirror(y, getDef(y).get.asInstanceOf[Op[MUnit]])

        System.out.println("INSIDE: ")
        System.out.println(str(y2))
        metadata.get(y2).foreach { case (k, m) => System.out.println(c"  $k : $m") }
        y2
      }
      System.out.println("OUTSIDE: ")
      System.out.println(str(y2))
      metadata.get(y2).foreach { case (k, m) => System.out.println(c"  $k : $m") }
    }
  }
}
