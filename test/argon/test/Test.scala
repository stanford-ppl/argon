package argon.test

import org.scalatest.{FlatSpec, Matchers, ShouldMatchers}

import org.virtualized.{SourceContext, virtualize}
import argon.{AppCore, RunnerCore, Config}
import argon.ops._
import argon.utils.deleteExts
import argon.traversal.IRPrinter
import argon.codegen.scalagen._

trait TestOps extends BoolOps with IfThenElseOps with PrintOps with TextOps with MixedNumericOps
trait TestApi extends TestOps with BoolApi with IfThenElseApi with PrintApi with TextApi with MixedNumericApi
trait TestExp extends TestOps with BoolExp with IfThenElseExp with PrintExp with TextExp with MixedNumericExp

trait ScalaGen extends ScalaCodegen with ScalaSingleFileGen
      with ScalaGenBool with ScalaGenIfThenElse with ScalaGenPrint with ScalaGenText with ScalaGenMixedNumeric
      with ScalaGenVoid {
  override val IR: TestExp
}

trait App extends AppCore with TestApi
trait CompilerBase extends RunnerCore with TestExp { self =>

  override val testbench = true

  lazy val printer = new IRPrinter { override val IR: CompilerBase.this.type = CompilerBase.this }

  printer.verbosity = 3

  passes += printer

  override def settings() {
    Config.verbosity = 3
    super.settings()
  }
}
trait Test extends CompilerBase with App {
  lazy val scalagen = new ScalaGen { override val IR: Test.this.type = Test.this }
  passes += scalagen
}


object Test1 extends Test {
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    println(x && y)
  }
}

object Test2 extends Test {
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test3 extends Test {
  @virtualize
  def main() {
    val x = !random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test4 extends Test {
  @virtualize
  def main() {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test5 extends Test {
  @virtualize
  def main() {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) random[Boolean] else random[Boolean]
    println(y)
  }
}

object Test6 extends Test {
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = if (x) x && y else x || y
    println(z)
  }
}

object Test7 extends Test {
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = if (x) { if (x && y) random[Boolean] else if (x || y) random[Boolean] else random[Boolean] } else lift(true)
    println(z)
  }
}

object Test8 extends Test {
  @virtualize
  def main() {
    val x = random[Int]
    val y = random[Int]
    val z = if (x == y) 32 else 0
    println(x + 0)
    println(0 + x)
    println(-(-x))
    println(z)
  }
}

object Test9 extends Test {
  @virtualize
  def main() {
    val x = random[Int]
    val y = random[Int]
    println(!(x != y))
  }
}

object OverflowLiftTest extends Test {
  @virtualize
  def main() {
    type Nibble = FixPt[TRUE,_4,_0]

    val c = 100
    val x = random[Nibble] + c
    println(x)
  }
}
object UnderflowLiftTest extends Test {
  @virtualize
  def main() {
    type Nibble = FixPt[TRUE,_4,_0]
    val c = -100
    val x = random[Nibble] + c
    println(x)
  }
}

object IgnoreOverflowTest extends Test {
  @virtualize
  def main() {
    val c = 2147483648L
    val x = random[Int] + c.as[Int]
    println(x)
  }
}

object SimpleCastTest extends Test {
  @virtualize
  def main() {

    val x = random[Int]
    val y = x.to[Float]

    val m = random[Float]
    val n = m.to[Int]
    println(x)
    println(y)
    println(m)
    println(n)
  }
}


class Testbench extends FlatSpec with Matchers with argon.core.Exceptions {
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
  "SimpleCaseTest" should "compile" in { SimpleCastTest.main(noargs) }
  a [TestBenchFailed] should be thrownBy { OverflowLiftTest.main(noargs) }
  a [TestBenchFailed] should be thrownBy { UnderflowLiftTest.main(noargs) }
  "IgnoreOverflowTest" should "compile" in { IgnoreOverflowTest.main(noargs) }

  "SimpleMap2" should "compile" in { SimpleMap2.main(noargs) }
}
