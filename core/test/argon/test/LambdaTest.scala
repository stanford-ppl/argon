package argon.test

import argon.core.compiler._
import argon.compiler._
import argon.codegen.scalagen.ScalaCodegen
import forge._
import org.virtualized._

object SimpleLambdaOps {
  /** Constructors **/
  @internal def map2[T:Type](n: Exp[Int32], map1: Exp[Int32] => Exp[T], map2: Exp[T] => Exp[T], i: Bound[Int32]): Sym[T] = {
    val m1Blk = stageLambda1(i){ map1(i) }
    val m2Blk = stageLambda1(m1Blk.result) { map2(m1Blk.result) }
    val effects = m1Blk.effects andAlso m2Blk.effects
    stageEffectful(Map2(n, m1Blk, m2Blk, i), effects.star)(ctx)
  }
}

trait SimpleLambdaApi {
  // Contrived example - unfused map which only returns the first value
  // Keep both blocks without having to introduce extra bound variable
  @api def map[T:Type](n: Int32)(map1: Int32 => T)(map2: T => T): T = wrap {
    SimpleLambdaOps.map2(n.s, {i: Exp[Int32] => map1(wrap(i)).s}, {x: Exp[T] => map2(wrap(x)).s}, fresh[Index])
  }
}

/** IR Nodes **/
case class Map2[T: Type](n: Exp[Int32], map1: Lambda1[Int32,T], map2: Lambda1[T,T], i: Bound[Int32]) extends Op[T] {
  def mirror(f: Tx) = SimpleLambdaOps.map2(f(n), f(map1), f(map2), i)
  override def binds = super.binds :+ i
}

trait ScalaGenLambda extends ScalaCodegen {
  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case Map2(n, map1, map2, i) =>
      open(src"val $lhs = List.tabulate($n){$i => ")
      visitBlock(map1)
      emitBlock(map2)
      close("}.head")
    case _ => super.emitNode(lhs, rhs)
  }
}

case class ScalaGenLambdaTest(IR: State) extends ScalaGenBase with ScalaGenLambda


object LambdaApi extends TestApi with SimpleLambdaApi
trait LambdaTest extends TestBase {
  override protected def createTraversalSchedule(state: State) = {
    lazy val scalagen = ScalaGenLambdaTest(state)

    super.createTraversalSchedule(state)
    passes += scalagen
  }
}


object SimpleMap2 extends LambdaTest {
  import LambdaApi._
  def main() {
    val x = map(32){i => 5*i + 1}{x => x * 2}
    println(x)
  }
}
