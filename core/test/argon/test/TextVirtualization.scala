package argon.test

import org.virtualized._

object TextVirtualization extends Test {
  import api._

  def main() {}

  @virtualize
  override def main(args: scala.Array[java.lang.String]) = {
    super.main(args)

    IR.context = Nil

    val x = Array.empty[Int](32)
    val y = x(15)

    val q = println(y + " ")

    IR.context.foreach{s => System.out.println(s"${str(s)}")}
  }

}
