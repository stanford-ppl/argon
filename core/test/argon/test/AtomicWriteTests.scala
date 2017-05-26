package argon.test

import argon.core.{Config, TestBenchFailed}
import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._

object ArrayAtomicWrite extends Test {
  import argon.test.api._

  @virtualize
  def main() {
    val x = Array.empty[Array[Int]](32)

    Array.tabulate(32){i => x(i) = Array.fill(16){ 0.to[Int] } }

    x(0)(1) = 3

    println("" + x(0).apply(1))
  }
}

object ArrayNoAtomicWrite extends Test {
  import argon.test.api._

  @virtualize
  def main() {
    // HACK
    Config.allowAtomicWrites = false

    val x = Array.empty[Array[Int]](32)

    Array.tabulate(32){i => x(i) = Array.fill(16){ 0.to[Int] } }

    x(0)(1) = 3

    println("" + x(0).apply(1))
  }
}


class AtomicWriteTests extends FlatSpec with Matchers {

  "Atomic Writes" should "fail when disabled" in {
    a[TestBenchFailed] should be thrownBy {
      ArrayNoAtomicWrite.main(Array.empty)
    }
  }

  "ArrayAtomicWrite" should "compile with atomic writes enabled" in {
    ArrayAtomicWrite.main(Array.empty)
  }
}
