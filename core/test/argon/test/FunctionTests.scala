package argon.test

import virtualized._
import org.scalatest.{FlatSpec, Matchers}

object ScalaFunTest extends Test {
  import argon.test.api._
  @virtualize
  def main() {
    val f: scala.Function1[Int, Int] = (arg: Int) => {
      arg + 4
    }
    val f4 = f(4)
    assert(f4 == 8)
    println("Scala functions appears to be working")
  }
}
object ArgonFunTest extends Test {
  import argon.test.api._
  @virtualize
  def main() {
    val f: Function1[Int, Int] = (arg: Int) =>
      arg + 4
    assert(f(4) == 8)
    println("Argon functions appears to be working")
  }
}

object ArgonFunMapTest extends Test {
  import argon.test.api._
  @virtualize
  def main() {
    val a = Array.tabulate(16){i => i}
    val f: Function1[Int, Int] = (arg: Int) =>
      arg + 4
    val b = a.map(f)
    assert(b(15) == 15 + 4)
    println("FlatMap appears to be working")
  }
}
class FunctionTests extends FlatSpec with Matchers {
  val noargs = Array[String]()
  "ScalaFunTest" should "compile" in { ScalaFunTest.main(noargs) }
  "ArgonFunTest" should "compile" in { ArgonFunTest.main(noargs) }
  "ArgonFunMapTest" should "compile" in { ArgonFunMapTest.main(noargs) }
}
