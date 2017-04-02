package argon.test

import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._

class IfThenElseTest extends FlatSpec with Matchers{

  "Whatever" should "be staged" in {
    class IfThenElseTest extends Test {
      import IR._

      @virtualize
      def main() {
        val x = random[Int]
        val y = random[Int]
        val a = if (x == y) x else 32
        val b = if (y > x)  5 else x
        val c = if (y < x)  0 else 1
        val d = if (x != y) x else y
        println(a)
        println(b)
        println(c)
        println(d)
      }
    }
    (new IfThenElseTest).main(Array.empty)
  }

}
