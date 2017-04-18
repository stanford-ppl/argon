package argon.test
import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._

class StructTests extends FlatSpec with Matchers {

  "StructTest" should "compile" in {
    class StructTest extends Test {
      import IR._

      @struct class MyStruct(x: Int, y: Int)

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
    (new StructTest).main(Array.empty)
  }

  "StructDefaultArgsTest" should "compile" in {
    class StructTest extends Test {
      import IR._

      @struct class MyStruct(x: Int = 13, y: Int = 12)

      @virtualize
      def main() {
        val array = Array.tabulate(10){i => MyStruct(x = 25) }

        val m = array(0).x // TODO: Why is this a problem when the val's name is 'x'? Not an issue in normal Scala
        val y = array(0).y

        println("Expected: 25")
        println("X value: " + m)
        println("Y value: " + y)
      }
    }
    (new StructTest).main(Array.empty)
  }

}
