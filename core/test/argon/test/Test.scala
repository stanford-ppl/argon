package argon.test

import org.scalatest.{FlatSpec, Matchers}
import org.virtualized.{SourceContext, virtualize}
import argon._
import argon.ops._
import argon.traversal.IRPrinter
import argon.codegen.scalagen._
import argon.transform.ForwardTransformer
import forge._

import scala.runtime._

trait TestExp extends ArgonExp
  with AssertExp with PrintExp

trait LowPriorityImplicits {
  implicit def int2RichInt(x: Int): RichInt = new RichInt(x)
  implicit def long2RichLong(x: Long): RichLong = new RichLong(x)
  implicit def float2RichFloat(x: Float): RichFloat = new RichFloat(x)
  implicit def double2RichDouble(x: Double): RichDouble = new RichDouble(x)
}

trait TestApi extends TestExp with ArgonApi with LowPriorityImplicits
  with AssertApi with PrintApi {

  implicit class intWrapper(x: scala.Int) extends {
    @api def to[B:Meta](implicit cast: Cast[scala.Int,B]): B = cast(x)
  }
  implicit class longWrapper(x: scala.Long) {
    @api def to[B:Meta](implicit cast: Cast[scala.Long,B]): B = cast(x)
  }
  implicit class floatWrapper(x: scala.Float) {
    @api def to[B:Meta](implicit cast: Cast[scala.Float,B]): B = cast(x)
  }
  implicit class doubleWrapper(x: scala.Double) {
    @api def to[B:Meta](implicit cast: Cast[scala.Double,B]): B = cast(x)
  }
}

trait ScalaGen extends ScalaCodegen with ScalaFileGen
  with ScalaGenArray with ScalaGenArrayExt with ScalaGenAssert with ScalaGenBool with ScalaGenFixPt with ScalaGenFltPt
  with ScalaGenHashMap with ScalaGenIfThenElse with ScalaGenPrint with ScalaGenStructs
  with ScalaGenText with ScalaGenVoid {
  override val IR: TestExp
}

trait IdentityTransformer extends ForwardTransformer {
  override val name = "Identity Transformer"
}

trait CompilerBase extends RunnerCore with TestApi { self =>

  override val testbench = true

  lazy val printer  = new IRPrinter { override val IR: self.type = self }
  lazy val identity = new IdentityTransformer { override val IR: self.type = self }

  override def createTraversalSchedule() = {
    printer.verbosity = 3
    passes += printer
    passes += identity
    passes += printer
  }

  override def settings() {
    Config.verbosity = 3
    super.settings()
  }
}
trait TestIR extends CompilerBase { self =>
  lazy val scalagen = new ScalaGen { override val IR: self.type = self }

  override def createTraversalSchedule() = {
    super.createTraversalSchedule()
    passes += scalagen
  }

}
trait TestLib extends LibCore

trait Test extends AppCore {
  val IR: TestIR = new TestIR { }
  val Lib: TestLib = new TestLib { }
}


object Test1 extends Test {
  import IR._
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    println(x && y)
  }
}

object Test2 extends Test {
  import IR._
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test3 extends Test {
  import IR._
  @virtualize
  def main() {
    val x = !random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test4 extends Test {
  import IR._
  @virtualize
  def main() {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) false else true
    println(y)
  }
}

object Test5 extends Test {
  import IR._
  @virtualize
  def main() {
    val x = random[Boolean] && random[Boolean]
    val y = if (x) random[Boolean] else random[Boolean]
    println(y)
  }
}

object Test6 extends Test {
  import IR._
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = if (x) x && y else x || y
    println(z)
  }
}

object Test7 extends Test {
  import IR._
  @virtualize
  def main() {
    val x = random[Boolean]
    val y = random[Boolean]
    val z = {
      if (x) {
        if (x && y) random[Boolean]
        else if (x || y) random[Boolean]
        else random[Boolean]
      }
      else true
    }

    println(z)
  }
}

object Test8 extends Test {
  import IR._
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
  import IR._
  @virtualize
  def main() {
    val x = random[Int]
    val y = random[Int]
    println(!(x != y))
  }
}

object OverflowLiftTest extends Test {
  import IR._
  @virtualize
  def main() {
    type Nibble = FixPt[TRUE,_4,_0]

    val c = 100
    val x = random[Nibble] + c
    println(x)
  }
}
object UnderflowLiftTest extends Test {
  import IR._
  @virtualize
  def main() {
    type Nibble = FixPt[TRUE,_4,_0]
    val c = -100
    val x = random[Nibble] + c
    println(x)
  }
}

object IgnoreOverflowTest extends Test {
  import IR._
  @virtualize
  def main() {
    val c = 2147483648L
    val x = random[Int] + c.to[Int]
    println(x)
  }
}

object SimpleCastTest extends Test {
  import IR._
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

object UnstagedToStringTest extends Test {
  import IR._
  @virtualize
  def main() {
    val x = 0
    println(x.toString)
  }
}


class Testbench extends FlatSpec with Matchers with argon.core.Exceptions {
  val noargs = Array[String]()
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

  "UnstagedToStringTest" should "compile" in { UnstagedToStringTest.main(noargs) }

  "SimpleMap2" should "compile" in { SimpleMap2.main(noargs) }
}
