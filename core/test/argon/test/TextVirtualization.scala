package argon.test

import org.virtualized._

object TextVirtualization extends Test {
  import IR._

  def main() {}

  @virtualize
  override def main(args: scala.Array[java.lang.String]) = {
    context = Nil

    val x = Array.empty[Int](32)
    val y = x(15)

    val q = println(y + " ")

    context.foreach{s => System.out.println(s"${str(s)}")}
  }

}
