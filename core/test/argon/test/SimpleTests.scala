package argon.test

import org.scalatest.{FlatSpec, Matchers}
import virtualized._
import argon.TestBenchFailed

object Test1 extends Test {
  import api._
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    println(x && y)
  }
}

object Test2 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test3 extends Test {
  import api._
  @virtualize
  def main() {
    val x = !random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test4 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test5 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) random[Boolean] else random[Boolean]
    println(y)
  }
}

object Test6 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = if (x) x && y else x || y
    println(z)
  }
}

object Test7 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = {
      if (x) {
        if (x && y) random[Boolean]
        else if (x || y) random[Boolean]
        else random[Boolean]
      }
      else true
    }

    println(z)
  }
}

object Test8 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Int]
    val y = random[Int]
    val z = if (x == y) 32 else 0
    println(x + 0)
    println(0 + x)
    println(-(-x))
    println(z)
  }
}

object Test9 extends Test {
  import api._
  @virtualize
  def main() {
    val x = random[Int]
    val y = random[Int]
    println(!(x != y))
  }
}

object OverflowLiftTest extends Test {
  import api._
  @virtualize
  def main() {
    type Nibble = FixPt[TRUE,_4,_0]

    val c = 100
    val x = random[Nibble] + c
    println(x)
  }
}
object UnderflowLiftTest extends Test {
  import api._
  @virtualize
  def main() {
    type Nibble = FixPt[TRUE,_4,_0]
    val c = -100
    val x = random[Nibble] + c
    println(x)
  }
}

object IgnoreOverflowTest extends Test {
  import api._
  @virtualize
  def main() {
    val c = 2147483648L
    val x = random[Int] + c.to[Int]
    println(x)
  }
}

object SimpleCastTest extends Test {
  import api._
  @virtualize
  def main() {

    val x = random[Int]
    val y = x.to[Float]

    val m = random[Float]
    val n = m.to[Int]
    println(x)
    println(y)
    println(m)
    println(n)
  }
}

object UnstagedToStringTest extends Test {
  import api._
  @virtualize
  def main() {
    val x = 0
    println(x.toString)
  }
}

object StagedStringTest extends Test {
  import api._

  @virtualize def main(): Unit = {
    val cst1 = 32
    val cst2 = 23
    val cst3 = 11
    val cst4 = 7
    val g1 = cst1 + 2
    val g2 = cst2 + 4
    val g3 = cst3 + 6
    val g4 = cst4 + 8

    val data = Array.tabulate(32){i => i}
    val g6 = data(3)
    println("expected: " + g1 + ", " + g2 + ", " + g3 + ", " + g4 + ", "+ g6.toString)
  }
}

// Argon doesn't have codegen of arbitrary precision fixed point numbers for now
/*object FixPtTest extends Test {
  import api._
  type Q16 = FixPt[TRUE,_16,_16]

  @virtualize def main(): Unit = {
    val x = 0.32f.to[Q16]
    println(x)
  }
}*/

class StringStagingTests extends FlatSpec with Matchers {
  "StagedStringTest" should "compile" in {
    import argon.nodes.ToString
    StagedStringTest.main(Array.empty)
    val IR = StagedStringTest.IR.graph
    val tostr = IR.NodeData.value.collect{case d: ToString[_] => d }
    tostr.length should be >= 2
  }
}


class SimpleTests extends FlatSpec with Matchers {
  val noargs = Array[String]()
  "Test1" should "compile" in { Test1.main(noargs) }
  "Test2" should "compile" in { Test2.main(noargs) }
  "Test3" should "rewrite ifThenElse" in { Test3.main(noargs) }
  "Test4" should "compile" in { Test4.main(noargs) }
  "Test5" should "compile" in { Test5.main(noargs) }
  "Test6" should "compile" in { Test6.main(noargs) }
  "Test7" should "compile" in { Test7.main(noargs) }
  "Test8" should "compile" in { Test8.main(noargs) }
  "Test9" should "compile" in { Test9.main(noargs) }
  "SimpleCaseTest" should "compile" in { SimpleCastTest.main(noargs) }
  a [TestBenchFailed] should be thrownBy { OverflowLiftTest.main(noargs) }
  a [TestBenchFailed] should be thrownBy { UnderflowLiftTest.main(noargs) }
  "IgnoreOverflowTest" should "compile" in { IgnoreOverflowTest.main(noargs) }

  "UnstagedToStringTest" should "compile" in { UnstagedToStringTest.main(noargs) }

  "SimpleMap2" should "compile" in { SimpleMap2.main(noargs) }
  //"FixPtTest" should "compile" in { FixPtTest.main(noargs) }
}