package argon.test

import argon.ops._
import argon.Config
import argon.utils.deleteExts

import org.scalatest.{FlatSpec, Matchers, ShouldMatchers}

import scala.virtualized.{SourceContext, virtualize}

trait SimpleLambdaOps extends NumOps with FixPtOps {
  // Contrived example - unfused map
  // Keep both blocks without having to introduce extra bound variable
  def map[T:Staged](n: Int32)(map: Int32 => T)(map2: T => T)(implicit ctx: SrcCtx): T
}

trait SimpleLambdaApi extends SimpleLambdaOps with NumApi

trait SimpleLambdaExp extends SimpleLambdaOps with NumExp {

  def map[T:Staged](n: Int32)(map: Int32 => T)(map2: T => T)(implicit ctx: SrcCtx): T = {
    val i = fresh[Int32]
    val m1Blk = stageBlock{ map(wrap(i)).s }
    val m2Blk = stageLambda(m1Blk.result){ map2(wrap(m1Blk.result)).s }
    val effects = m1Blk.summary andAlso m2Blk.summary
    wrap(stageEffectful(Map2(n.s, m1Blk, m2Blk, i), effects.star)(ctx))
  }

  case class Map2[T:Staged](n: Sym[Int32], map1: Block[T], map2: Lambda[T], i: Sym[Int32]) extends Op[T] {
    def mirror(f:Tx) = op_map2(f(n),f(map1),f(map2), i)

    override def binds = super.binds :+ i
  }

  def op_map2[T:Staged](n: Sym[Int32], map1: => Sym[T], map2: => Sym[T], i: Sym[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    val m1Blk = stageBlock{ map1 }
    val m2Blk = stageLambda(m1Blk.result){ map2 }
    val effects = m1Blk.summary andAlso m2Blk.summary
    stageEffectful(Map2(n, m1Blk, m2Blk, i), effects.star)(ctx)
  }

}

trait LambdaTest extends Test with SimpleLambdaExp

object SimpleReduction extends LambdaTest {
  def main() {
    val x = map(32){i => 5*i + 1}{x => x * 2}
    println(x)
  }
}

class ReduceTestbench extends FlatSpec with Matchers with argon.core.Exceptions {
  val noargs = Array[String]()
  deleteExts(Config.logDir, ".log")

  "SimpleReduction" should "compile" in { SimpleReduction.main(noargs) }
}