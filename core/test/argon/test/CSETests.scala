package argon.test

import org.scalatest.{FlatSpec, Matchers}
import org.virtualized._


object NoCSEAcrossCold extends Test {
  import IR._
  IR.testArgs = List("1")

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

class CSETests extends FlatSpec with Matchers with argon.core.Exceptions {
  val noargs = Array[String]()

  "NoCSEAcrossCold" should "not CSE across cold blocks" in {
    NoCSEAcrossCold.main(noargs)
    val nDivs = NoCSEAcrossCold.IR.NodeData.value.count{case _: NoCSEAcrossCold.IR.FixDiv[_,_,_] => true; case _ => false }

    nDivs should be >= 3
  }
}