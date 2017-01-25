package argon.test

import argon.ops._
import argon.{AppCore, Config, LibCore}
import argon.codegen.scalagen.ScalaCodegen
import argon.utils.deleteExts
import org.scalatest.{FlatSpec, Matchers, ShouldMatchers}
import org.virtualized.{SourceContext, virtualize}

trait SimpleLambdaOps extends NumOps with FixPtOps {
  // Contrived example - unfused map which only returns the first value
  // Keep both blocks without having to introduce extra bound variable
  def map[T:Staged](n: Int32)(map: Int32 => T)(map2: T => T)(implicit ctx: SrcCtx): T
}

trait SimpleLambdaApi extends SimpleLambdaOps with NumApi

trait SimpleLambdaExp extends SimpleLambdaOps with NumExp {
  def map[T: Staged](n: Int32)(map: Int32 => T)(map2: T => T)(implicit ctx: SrcCtx): T = {
    val i = fresh[Int32]
    val m1Blk = stageBlock {
      map(wrap(i)).s
    }
    val m2Blk = stageLambda(m1Blk.result) {
      map2(wrap(m1Blk.result)).s
    }
    val effects = m1Blk.summary andAlso m2Blk.summary
    wrap(stageEffectful(Map2(n.s, m1Blk, m2Blk, i), effects.star)(ctx))
  }

  case class Map2[T: Staged](n: Exp[Int32], map1: Block[T], map2: Block[T], i: Bound[Int32]) extends Op[T] {
    def mirror(f: Tx) = op_map2(f(n), f(map1), f(map2), i)
    override def binds = super.binds :+ i
  }

  def op_map2[T: Staged](n: Exp[Int32], map1: => Exp[T], map2: => Exp[T], i: Bound[Int32])(implicit ctx: SrcCtx): Sym[T] = {
    val m1Blk = stageBlock {
      map1
    }
    val m2Blk = stageLambda(m1Blk.result) {
      map2
    }
    val effects = m1Blk.summary andAlso m2Blk.summary
    stageEffectful(Map2(n, m1Blk, m2Blk, i), effects.star)(ctx)
  }
}


trait ScalaGenLambda extends ScalaCodegen {
  val IR: SimpleLambdaExp
  import IR._

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Map2(n, map1, map2, i) =>
      open(src"val $lhs = List.tabulate($n){$i => ")
      visitBlock(map1)
      emitBlock(map2)
      close("}.head")
    case _ => super.emitNode(lhs, rhs)
  }
}

trait ScalaGenLambdaTest extends ScalaGen with ScalaGenLambda { override val IR: LambdaTestIR }

trait LambdaTestIR extends CompilerBase with SimpleLambdaExp { self =>
  val scalagen = new ScalaGenLambdaTest{val IR: self.type = self }
  passes += scalagen
}

trait LambdaTestLib extends LibCore

trait LambdaTest extends AppCore {
  val IR: LambdaTestIR = new LambdaTestIR { }
  val Lib: LambdaTestLib = new LambdaTestLib { }
}

object SimpleMap2 extends LambdaTest {
  import IR._
  def main() {
    val x = map(32){i => 5*i + 1}{x => x * 2}
    println(x)
  }
}
