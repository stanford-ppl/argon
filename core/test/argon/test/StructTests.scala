package argon.test

import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._
import api._

@struct case class MyStruct(x: Int, y: Int)
@struct case class MyStruct2(x: Int = 13, y: Int = 12)

class StructTests extends FlatSpec with Matchers {

  "StructTest" should "compile" in {

    class StructTest extends Test {
      @virtualize
      def main() {
        val array = Array.tabulate(32){i => MyStruct(i + 1, i - 1) }

        val x = array(10).x
        val y = random[MyStruct]

        val sum = array.reduce{(a,b) => a + b}

        println("Expected: 11")
        println("Value: " + x)
        println("Random: " + y)
        println("Sum: " + sum)
      }
    }
    (new StructTest).main(scala.Array.empty)
  }

  "StructDefaultArgsTest" should "compile" in {
    class StructTest extends Test {
      @virtualize
      def main() {
        val array = Array.tabulate(10){i => MyStruct2(x = 25) }

        val m = array(0).x // TODO: Why is this a problem when the val's name is 'x'? Not an issue in normal Scala
        val y = array(0).y

        println("Expected: 25")
        println("X value: " + m)
        println("Y value: " + y)
      }
    }
    (new StructTest).main(scala.Array.empty)
  }

  "CopyTest" should "compile" in {
    class StructCopyTest extends Test {
      @virtualize def main(): Unit = {
        val x = MyStruct(1, 2)
        val a = x.copy(x = 3, y =4)
        val b = x.copy(x = 1)
        val c = x.copy(y = 1)
        val d = x.copy()
        println(a)
        println(b)
        println(c)
        println(d)
      }
    }
    (new StructCopyTest).main(scala.Array.empty)
  }

}
