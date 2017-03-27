package argon.test

import argon.transform.ForwardTransformer
import org.virtualized.SourceContext

/**
  * Created by david on 1/30/17.
  */


object MirroringTest extends Test { self =>
  import IR._

  override def main() = {}

  // Note: All of this internal stuff shouldn't necessarily be visible to the user, just need it for debugging for now
  override def main(args: scala.Array[java.lang.String]) = {
    context = Nil
    argon.Config.verbosity = 3
    withLog(argon.Config.logDir, "MirroringTest.log") {
      val x = Array.empty[Int](1)
      val y = array_update(x.s, int32(0), int32(16))
      val z = Array.empty[Int](2)
      val tx = new ForwardTransformer {
        val IR: self.IR.type = self.IR
      }

      val y2 = tx.withSubstScope(x.s -> z.s) {
        val y2 = tx.mirror(y, getDef(y).get.asInstanceOf[Op[Void]])

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
