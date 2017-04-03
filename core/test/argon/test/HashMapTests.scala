package argon.test

import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._

object GroupByReduceTest extends Test {
  import IR._

  @virtualize
  def main() {
    val array = Array.tabulate(50){i => random[Int](50) }

    val map = array.groupByReduce{x => x % 10.to[Int] }{x => 1.to[Int]}{_+_}
    val max = map.values.reduce{(a,b) => if (a > b) a else b}

    array.foreach{x => print("" + x + " ") }
    println("")
    println("max: " + max)

    // Super awesome console histogram
    // Hack: need range syntax in argon
    Array.tabulate(max){i => i}.foreach{m =>
      map.keys.foreach { k =>
        if (map(k) >= max - m) print("X  ") else print("   ")
      }
      println("")
    }
    map.keys.foreach{k => print("" + k + "  ") }
    println("")
  }
}

class HashMapTests extends FlatSpec with Matchers {
  "GroupByReduceTest" should "compile and run" in { GroupByReduceTest.main(Array.empty) }
}
