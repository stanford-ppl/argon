package argon.test

import org.scalatest.FlatSpec
import scala.virtualized.{SourceContext, virtualize}
import argon.{AppCore, CompilerCore, Config}
import argon.ops._
import argon.utils.deleteExts
import argon.traversal.IRPrinter

trait App extends AppCore with BoolApi with IfThenElseApi with PrintApi with TextApi with VoidApi with FixPtApi
trait Compiler extends CompilerCore with BoolExp with IfThenElseExp with PrintExp with TextExp with FixPtExp { self =>
  lazy val printer = new IRPrinter { override val IR: Compiler.this.type = Compiler.this }
  printer.verbosity = 3

  passes += printer

  override def settings() {
    Config.verbosity = 3
    super.settings()
  }
}
trait Test extends Compiler with App


object Test1 extends Test {
  def main(): Void = {
    val x = random[Boolean]
    val y = random[Boolean]
    println(x && y)
  }
}

object Test2 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Boolean]
    val y = if (x) lift(false) else lift(true)
    println(y)
  }
}

object Test3 extends Test {
  @virtualize
  def main(): Void = {
    val x = !random[Boolean]
    val y = if (x) lift(false) else lift(true)
    println(y)
  }
}

object Test4 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) lift(false) else lift(true)
    println(y)
  }
}

object Test5 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) random[Boolean] else random[Boolean]
    println(y)
  }
}

object Test6 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = if (x) x && y else x || y
    println(z)
  }
}

object Test7 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = if (x) { if (x && y) randomBool() else if (x || y) randomBool() else randomBool() } else lift(true)
    println(z)
  }
}

object Test8 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Int]
    val y = random[Int]
    println(x + 0)
    println(0 + x)
    println(-(-x))
  }
}

object Test9 extends Test {
  @virtualize
  def main(): Void = {
    val x = random[Int]
    val y = random[Int]
    println(!(x != y))
  }
}



class Testbench extends FlatSpec {
  val noargs = Array[String]()
  deleteExts(Config.logDir, ".log")

  "Test1" should "compile" in { Test1.main(noargs) }
  "Test2" should "compile" in { Test2.main(noargs) }
  "Test3" should "rewrite ifThenElse" in { Test3.main(noargs) }
  "Test4" should "compile" in { Test4.main(noargs) }
  "Test5" should "compile" in { Test5.main(noargs) }
  "Test6" should "compile" in { Test6.main(noargs) }
  "Test7" should "compile" in { Test7.main(noargs) }
  "Test8" should "compile" in { Test8.main(noargs) }
  "Test9" should "compile" in { Test9.main(noargs) }

}
