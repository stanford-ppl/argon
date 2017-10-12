package argon.test

import argon.ArgonAppRunner
import argon.core._
import argon.codegen.scalagen._
import argon.lang.cake.ArgonLangExternal
import argon.lang.direct.{AssertApi, PrintApi}
import argon.transform.ForwardTransformer
import argon.traversal.IRPrinter
import forge._

import scala.runtime._

trait LowPriorityImplicits {
  implicit def int2RichInt(x: Int): RichInt = new RichInt(x)
  implicit def long2RichLong(x: Long): RichLong = new RichLong(x)
  implicit def float2RichFloat(x: Float): RichFloat = new RichFloat(x)
  implicit def double2RichDouble(x: Double): RichDouble = new RichDouble(x)
}

trait TestApi extends ArgonLangExternal with LowPriorityImplicits
  with PrintApi
  with AssertApi
{

  implicit class intWrapper(x: scala.Int) extends {
    @api def to[B:Type](implicit cast: Cast[scala.Int,B]): B = cast(x)
  }
  implicit class longWrapper(x: scala.Long) {
    @api def to[B:Type](implicit cast: Cast[scala.Long,B]): B = cast(x)
  }
  implicit class floatWrapper(x: scala.Float) {
    @api def to[B:Type](implicit cast: Cast[scala.Float,B]): B = cast(x)
  }
  implicit class doubleWrapper(x: scala.Double) {
    @api def to[B:Type](implicit cast: Cast[scala.Double,B]): B = cast(x)
  }
}

/** In DSLs, this can be the package object for the top package **/
object api extends TestApi

trait ScalaGenBase extends ScalaCodegen with ScalaFileGen
  with ScalaGenArray with ScalaGenAssert with ScalaGenBoolean with ScalaGenFixPt with ScalaGenFltPt
  with ScalaGenHashMap with ScalaGenIfThenElse with ScalaGenPrint with ScalaGenStructs
  with ScalaGenString with ScalaGenUnit with ScalaGenFunction with ScalaGenVariables

case class IdentityTransformer(var IR: State) extends ForwardTransformer {
  override val name = "Identity Transformer"
}

trait TestBase extends ArgonAppRunner {
  override val testbench = true

  override protected def createTraversalSchedule(state: State) = {
    lazy val printer  = IRPrinter(state)
    lazy val identity = IdentityTransformer(state)

    printer.verbosity = 3
    passes += printer
    passes += identity
    passes += printer
  }

  override def settings() {
    config.verbosity = 3
    super.settings()
  }
}

case class ScalaGen(var IR: State) extends ScalaGenBase

trait Test extends TestBase {
  override protected def createTraversalSchedule(state: State) = {
    super.createTraversalSchedule(state)

    lazy val scalagen = ScalaGen(state)
    passes += scalagen
  }
}

