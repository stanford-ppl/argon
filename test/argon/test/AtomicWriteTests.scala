package argon.test

import argon.Config
import argon.core.Exceptions
import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._

object ArrayAtomicWrite extends Test {
  import IR._

  @virtualize
  def main() {
    val x = Array[Array[Int]](32)

    Array.tabulate(32){i => ArrayInfixOps(x)(i) = Array.fill(16){ 0.as[Int] } } // hack...

    x(0)(1) = 3

    println("" + x(0).apply(1))
  }
}


class AtomicWriteTests extends FlatSpec with Matchers with Exceptions {

  "ArrayAtomicWrite" should "compile with atomic writes enabled" in {
    Config.allowAtomicWrites = true
    ArrayAtomicWrite.main(Array.empty)
  }

  "Atomic Writes" should "fail when disabled" in {
    a [TestBenchFailed] should be thrownBy {
      Config.allowAtomicWrites = false
      ArrayAtomicWrite.main(Array.empty)
    }
  }
}
