package argon.test

import org.virtualized.virtualize
import org.virtualized.SourceContext
import org.scalatest.{FlatSpec, Matchers}

object UpdateTest extends Test {
  import IR._
  @virtualize
  def main() {
    val array = Array.empty[Int](16)
    array(5) = 3
    assert(array(5) == 3)
    println("Array update appears to be working")
  }
}

object FillTest extends Test {
  import IR._
  @virtualize
  def main() {
    val array = Array.fill(10){ random[Int] }
    assert(array(0) != array(1))
    println("Array tabulate appears to be working")
  }
}

object TabulateTest extends Test {
  import IR._
  @virtualize
  def main() {
    val array = Array.tabulate(16){i => i + 1 }

    // Unstaged loop
    for (i <- 0 until 15) {
      assert(array(i) == i + 1)
    }
    println("Array tabulate appears to be working")
  }
}

object ForeachTest extends Test {
  import IR._
  @virtualize
  def main() {
    val array = Array.tabulate(16){i => random[Int](10) }
    array.foreach{x => println(x) }
    array.foreach{x => assert(x < 10 && x > -10) }
    println("Foreach appears to be working")
  }
}

object MapTest extends Test {
  import IR._
  @virtualize
  def main() {
    val array = Array.tabulate(16){i => random[Int](10) }
    val array2 = array.map{x => x + 5 }

    for (i <- 0 until 16) {
      assert(array2(i) - array(i) == 5)
    }
    println("Map appears to be working")
  }
}

object ZipTest extends Test {
  import IR._
  @virtualize
  def main() {
    val a = Array.tabulate(16){i => random[Int](10) }
    val b = Array.tabulate(16){i => random[Int](10) }
    val c = a.zip(b){(x,y) => x + y}
    for (i <- 0 until 10) {
      assert(a(i) + b(i) == c(i))
    }
    println("Zip appears to be working")
  }
}

object ReduceTest extends Test {
  import IR._
  @virtualize
  def main() {
    val a = Array.tabulate(16){i => i}
    val sum = a.reduce{(x,y) => x + y}
    assert(sum == 120)
    println("Reduce appears to be working")
  }
}

object FilterTest extends Test {
  import IR._
  @virtualize
  def main() {
    val a = Array.tabulate(16){i => i}
    val b = a.filter{x => x % 2 == 0}

    b.foreach{x => assert(x % 2 == 0) }
    println("Filter appears to be working")
  }
}

object FlatMapTest extends Test {
  import IR._
  @virtualize
  def main() {
    val a = Array.tabulate(16){i => i}
    val b = a.flatMap{x => Array[Int](x) }
    assert(b.length == 120)
    println("FlatMap appears to be working")
  }
}

class ArrayTests extends FlatSpec with Matchers with argon.core.Exceptions {
  val noargs = Array[String]()
  "UpdateTest" should "compile" in { UpdateTest.main(noargs) }
  "FillTest" should "compile" in { FillTest.main(noargs) }
  "TabulateTest" should "compile" in { TabulateTest.main(noargs) }
  "ForeachTest" should "compile" in { ForeachTest.main(noargs) }
  "MapTest" should "compile" in { MapTest.main(noargs) }
  "ZipTest" should "compile" in { ZipTest.main(noargs) }
  "ReduceTest" should "compile" in { ReduceTest.main(noargs) }
  "FilterTest" should "compile" in { FilterTest.main(noargs) }
  "FlatMapTest" should "compile" in { FlatMapTest.main(noargs) }
}
