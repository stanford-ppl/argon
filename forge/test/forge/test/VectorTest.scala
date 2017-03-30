package forge.test

import forge._

object VectorTest {

  trait Vector[T] { def s: Int }

  @generate //("II <- 1 to 2")
  case class VectorII$II$1to2[T](s: Int) extends Vector[T] {
    def testII: Int = II
  }

  @generate //("II <- 1 to 2, JJ <- 1 to II")
  case class NestedII$II$1to2[TJJ$JJ$1toII](xJJ$JJ$1toII: TJJ) {
    def _JJ$JJ$1toII = xJJ
    def elems = Seq(xJJ$JJ$1toII)
  }

  @generate //("II <- 1 to 2, JJ <- 1 to II")
  def test(x: Any): Int = x match {
    case x: NestedII$II$1to2[xJJ$JJ$1toII] => II
    case _ => 0
  }

  def main(args: Array[String]): Unit = {
    println("hello")
  }
}