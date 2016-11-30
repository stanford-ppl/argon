package argon.ops

import argon.core.Base
import scala.language.implicitConversions

trait BoolOps extends Base with NumAPI {
  type Bool <: BoolInfixOps
  /** Infix operations **/
  protected trait BoolInfixOps {
    def &&(that: Bool)(implicit ctx: SrcCtx): Bool
    def ||(that: Bool)(implicit ctx: SrcCtx): Bool
    def unary_!(implicit ctx: SrcCtx): Bool
  }

  /** Direct operations **/
  def randomBool()(implicit ctx: SrcCtx): Bool

  /** Internal **/
  implicit def lift(x: Boolean): Bool
  implicit val BoolType: Num[Bool]
}

trait BoolAPI extends BoolOps {
  type Boolean = Bool
}

trait BoolCore extends BoolOps with NumCore {
  /** Infix methods **/
  case class Bool() extends Sym with BoolInfixOps {
    override type LibType = Boolean
    def tp = BoolType.asInstanceOf[Typ[this.type]]

    def unary_!(implicit ctx: SrcCtx): Bool = stage(Not(this))(ctx)
    def &&(that: Bool)(implicit ctx: SrcCtx): Bool = stage(And(this,that))(ctx)
    def ||(that: Bool)(implicit ctx: SrcCtx): Bool = stage(Or(this,that))(ctx)
  }
  implicit object BoolType extends Num[Bool] {
    override def next: Bool = Bool()
    override def typeArguments = Nil
    override def stagedClass = classOf[Bool]
    override def isPrimitive = true

    lazy val one: Bool = fresh[Bool].asConst(true)
    lazy val zero: Bool = fresh[Bool].asConst(false)
    def random(implicit ctx: SrcCtx): Bool = randomBool()
  }

  /** Direct methods **/
  def randomBool()(implicit ctx: SrcCtx): Bool = stageSimple(RandomBool())(ctx)

  /** IR Nodes **/
  case class And(a: Bool, b: Bool) extends Op[Bool] { def mirror(f:Tx) = f(a) && f(b) }
  case class Or(a: Bool, b: Bool) extends Op[Bool] { def mirror(f:Tx) = f(a) || f(b) }
  case class Not(a: Bool) extends Op[Bool] { def mirror(f:Tx) = !f(a) }
  case class RandomBool() extends Op[Bool] { def mirror(f:Tx) = randomBool() }

  /** Rewrite rules **/
  rewrite[And]{
    case And(Const(x:Boolean), Const(y:Boolean)) => lift(x && y)   // Constant propagation
    case And(Const(false), _)                    => lift(false)    // Short circuit evaluation
    case And(_, Const(false))                    => lift(false)    // Short circuit evaluation
    case And(Const(true), b)                     => b              // Boolean simplification
    case And(a, Const(true))                     => a              // Boolean simplification
  }
  rewrite[Or] {
    case Or(Const(x:Boolean),Const(y:Boolean)) => lift(x || y)     // Constant propagation
    case Or(Const(true), _)                    => lift(true)       // Short circuit evaluation
    case Or(x, Const(true))                    => lift(true)       // Short circuit evaluation
    case Or(Const(false), y)                   => y                // Boolean simplification
    case Or(x, Const(false))                   => x                // Boolean simplification
  }
  rewrite[Not] {
    case Not(Const(x:Boolean)) => lift(!x)                         // Constant propagation
    case Not(Def(Not(x)))      => x                                // Boolean simplification
  }

  /** Evaluation **/
  eval[RandomBool]{ case _ => scala.util.Random.nextBoolean() }

  /** Internal methods **/
  implicit def lift(x: Boolean): Bool = fresh[Bool].asConst(x)
}
