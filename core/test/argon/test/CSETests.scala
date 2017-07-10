package argon.test

import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._

object NoCSEAcrossCold extends Test {
  import argon.test.api._
  testArgs = List("1")

  @virtualize
  def main() {
    val x = args(0).to[Int]

    if (x != 0) {
      println(2 / x)
    }

    if (x > 0) {
      println(2 / x)
    }

    println(2 / x)
  }
}

class CSETests extends FlatSpec with Matchers {
  val noargs = Array[String]()

  "NoCSEAcrossCold" should "not CSE across cold blocks" in {
    import argon.nodes.FixDiv
    NoCSEAcrossCold.main(noargs)

    val IR = NoCSEAcrossCold.IR.graph
    val nDivs = IR.NodeData.value.count{case _: FixDiv[_,_,_] => true; case _ => false }

    nDivs should be >= 3
  }
}