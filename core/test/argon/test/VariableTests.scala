package argon.test

import argon.core.Exceptions
import org.scalatest.{FlatSpec, Matchers}

import org.virtualized._

object NestedArrayTest extends Test {
  import IR._

  type T = Float

  @virtualize def main(): Unit = {
    val N = 10

    val sigReal = Array.tabulate(N){ i => i }

    /* Initialize DFT matrices (for real and imaginary terms). */
    val DFTReal = Array.tabulate(N){i =>
      Array.tabulate(N){j =>
        val nextEntr = -i.to[T]*j.to[T]*(2.0 / N.to[Float]).to[T]
        nextEntr.to[Float].to[T]
      }
    }

    for (row <- DFTReal) {
      for (elem <- row) {
        print(elem + ", ")
      }
      println()
    }


    val reRe = Array.empty[T](N)

    var p = 0 // NOTE: These are illegal right now (but we don't give an error yet)
    for (i <- DFTReal) {
      var nextEntr = 0.to[T]
      var q = 0
      for (j <- i) {
        nextEntr = nextEntr + j.to[T] * sigReal(q).to[T]
        q = q + 1
      }
      reRe(p) = nextEntr
      p = p + 1
    }

    for (e <- reRe) { println(e + ", ") }

  }
}


class VariableTests extends FlatSpec with Matchers with Exceptions {
  "NestedArrayTest" should "compile" in { NestedArrayTest.main(Array.empty) }

}
