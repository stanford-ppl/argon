package argon.ops

import argon.core.{Base, Core}
import scala.language.implicitConversions

trait BoolCommon {
  def rand_bool_impl(): Boolean = scala.util.Random.nextBoolean()
}

trait BoolCore extends Core with BoolCommon {
  /** Infix methods **/
  case class Bool() extends Sym {
    override type LibType = Boolean
    def tp = BoolType.asInstanceOf[Typ[this.type]]

    def &&(that: Bool)(implicit ctx: SrcCtx): Bool = stage(And(this,that))(ctx)
    def ||(that: Bool)(implicit ctx: SrcCtx): Bool = stage(Or(this,that))(ctx)
    def unary_!(implicit ctx: SrcCtx): Bool = stage(Not(this))(ctx)
  }
  implicit object BoolType extends Typ[Bool] {
    override def next: Bool = Bool()
    override def typeArguments = Nil
    override def stagedClass = classOf[Bool]
    override def isPrimitive = true
  }

  /** Implicit lifting **/
  implicit class LiftBooleanOps(a: Boolean) {
    def &&(b: Bool)(implicit ctx: SrcCtx): Bool = stage(And(lift(a),b))(ctx)
    def ||(b: Bool)(implicit ctx: SrcCtx): Bool = stage(Or(lift(a),b))(ctx)
  }

  implicit def lift(x: Boolean): Bool = fresh[Bool].asConst(x)

  /** IR Nodes **/
  case class And(a: Bool, b: Bool) extends Op[Bool] { def mirror(f:Tx) = f(a) && f(b) }
  case class Or(a: Bool, b: Bool) extends Op[Bool] { def mirror(f:Tx) = f(a) || f(b) }
  case class Not(a: Bool) extends Op[Bool] { def mirror(f:Tx) = !f(a) }
  case class RandomBool() extends Op[Bool] { def mirror(f:Tx) = __rand_bool() }

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
  eval[RandomBool]{ case _ => rand_bool_impl() }

  /** Internal methods **/
  def __rand_bool()(implicit ctx: SrcCtx): Bool = stageSimple(RandomBool())(ctx)
}

trait BoolOps extends Base {
  type Boolean
  def randomBool()(implicit ctx: SrcCtx): Boolean
}

trait BoolAPI extends BoolCore with BoolOps {
  type Boolean = Bool
  def randomBool()(implicit ctx: SrcCtx): Boolean = __rand_bool()(ctx)
}

trait BoolLib extends BoolOps with BoolCommon {
  type Boolean = scala.Boolean
  def randomBool()(implicit ctx: SrcCtx): Boolean = rand_bool_impl()
}
