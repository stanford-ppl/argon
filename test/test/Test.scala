package test

import org.scalatest.FlatSpec
import virtualized.{staged,SourceContext}
import argon.AppCore
import argon.ops._
import argon.utils.deleteExts
import argon.Config
import argon.traversal.IRPrinter

trait App extends AppCore with BoolAPI with IfThenElseAPI with PrintAPI with TextAPI with VoidAPI { self =>
  lazy val printer = new IRPrinter { override val IR: App.this.type = App.this }
  printer.verbosity = 3

  passes += printer

  override def settings() {
    Config.verbosity = 3
    super.settings()
  }
}


object Test1 extends App {
  def main(): Void = {
    val x = randomBool()
    val y = randomBool()
    println(x && y)
  }
}

object Test2 extends App {
  @staged
  def main(): Void = {
    val x = randomBool()
    val y = if (x) lift(false) else lift(true)
    println(y)
  }
}

object Test3 extends App {
  @staged
  def main(): Void = {
    val x = !randomBool()
    val y = if (x) lift(false) else lift(true)
    println(y)
  }
}

object Test4 extends App {
  @staged
  def main(): Void = {
    val x = randomBool() && randomBool()
    val y = if (x) lift(false) else lift(true)
    println(y)
  }
}

object Test5 extends App {
  @staged
  def main(): Void = {
    val x = randomBool() && randomBool()
    val y = if (x) randomBool() else randomBool()
    println(y)
  }
}

object Test6 extends App {
  @staged
  def main(): Void = {
    val x = randomBool()
    val y = randomBool()
    val z = if (x) x && y else x || y
    println(z)
  }
}

object Test7 extends App {
  @staged
  def main(): Void = {
    val x = randomBool()
    val y = randomBool()
    val z = if (x) { if (x && y) randomBool() else if (x || y) randomBool() else randomBool() } else lift(true)
    println(z)
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

}
