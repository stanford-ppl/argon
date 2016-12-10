package argon.ops
import argon.core.Base

trait Bools extends Base with NumApi {
  type Bool <: BoolOps
  /** Infix operations **/
  protected trait BoolOps {
    def unary_!(implicit ctx: SrcCtx): Bool
    def &&(that: Bool)(implicit ctx: SrcCtx): Bool
    def ||(that: Bool)(implicit ctx: SrcCtx): Bool
    def ^ (that: Bool)(implicit ctx: SrcCtx): Bool
  }

  implicit class BooleanBoolOps(x: Boolean) {
    def &&(y: Bool)(implicit ctx: SrcCtx): Bool = lift(x) && y
    def ||(y: Bool)(implicit ctx: SrcCtx): Bool = lift(x) || y
    def ^ (y: Bool)(implicit ctx: SrcCtx): Bool = lift(x) ^ y
  }

  /** Direct operations **/
  def randomBool()(implicit ctx: SrcCtx): Bool

  /** Internal **/
  implicit def lift(x: Boolean): Bool
  implicit val BoolType: Bits[Bool]
}

trait BoolApi extends Bools {
  type Boolean = Bool
}

trait BoolExp extends Bools with NumExp {
  /** Infix methods **/
  case class Bool(s: Sym[Bool]) extends BoolOps {
    def unary_!(implicit ctx: SrcCtx): Bool = stage(Not(this.s))(ctx)
    def &&(that: Bool)(implicit ctx: SrcCtx): Bool = stage(And(this.s,that.s))(ctx)
    def ||(that: Bool)(implicit ctx: SrcCtx): Bool = stage(Or(this.s,that.s))(ctx)
    def ^ (that: Bool)(implicit ctx: SrcCtx): Bool = infix_!=(this, that)(ctx)
  }
  def infix_==(x: Bool, y: Bool)(implicit ctx: SrcCtx): Bool = stage(XNor(x.s,y.s))(ctx)
  def infix_!=(x: Bool, y: Bool)(implicit ctx: SrcCtx): Bool = stage(XOr(x.s,y.s))(ctx)

  implicit object BoolType extends Bits[Bool] {
    override def wrap(x: Sym[Bool]): Bool = Bool(x)
    override def unwrap(x: Bool): Sym[Bool] = x.s
    override def typeArguments = Nil
    override def stagedClass = classOf[Bool]
    override def isPrimitive = true

    lazy val one: Bool = const[Bool](true)
    lazy val zero: Bool = const[Bool](false)
    def random(implicit ctx: SrcCtx): Bool = randomBool()
  }

  /** Direct methods **/
  def randomBool()(implicit ctx: SrcCtx): Bool = stageSimple(RandomBool())(ctx)

  /** IR Nodes **/
  case class Not(a: Sym[Bool]) extends Op[Bool] { def mirror(f:Tx) = !f(a) }
  case class And(a: Sym[Bool], b: Sym[Bool]) extends Op[Bool]  { def mirror(f:Tx) = f(a) && f(b) }
  case class Or(a: Sym[Bool], b: Sym[Bool]) extends Op[Bool]   { def mirror(f:Tx) = f(a) || f(b) }
  case class XOr(a: Sym[Bool], b: Sym[Bool]) extends Op[Bool]  { def mirror(f:Tx) = infix_!=(f(a), f(b)) }
  case class XNor(a: Sym[Bool], b: Sym[Bool]) extends Op[Bool] { def mirror(f:Tx) = infix_==(f(a), f(b)) }
  case class RandomBool() extends Op[Bool] { def mirror(f:Tx) = randomBool() }

  /** Rewrite rules **/
  rewrite[Not] {
    case Not(Const(x:Boolean)) => bool(!x)                           // Constant propagation
    case Not(Def(Not(x)))      => x                                  // Boolean simplification
  }
  rewrite[And]{
    case And(Const(x:Boolean), Const(y:Boolean)) => bool(x && y)     // Constant propagation
    case And(Const(false), _)                    => bool(false)      // Short circuit evaluation
    case And(_, Const(false))                    => bool(false)      // Short circuit evaluation
    case And(Const(true), b)                     => b                // Boolean simplification
    case And(a, Const(true))                     => a                // Boolean simplification
  }
  rewrite[Or] {
    case Or(Const(x:Boolean),Const(y:Boolean)) => bool(x || y)       // Constant propagation
    case Or(Const(true), _)                    => bool(true)         // Short circuit evaluation
    case Or(_, Const(true))                    => bool(true)         // Short circuit evaluation
    case Or(Const(false), y)                   => y                  // Boolean simplification
    case Or(x, Const(false))                   => x                  // Boolean simplification
  }
  rewrite[XOr] {
    case XOr(Const(x:Boolean),Const(y:Boolean)) => bool(x != y)           // Constant propagation
    case XOr(Const(false), y)                   => y                      // Boolean simplification (false != y)
    case XOr(Const(true), y)                    => stage(Not(y))(here).s  // Boolean simplification (true != y)
    case XOr(x, Const(false))                   => x                      // Boolean simplification (x != false)
    case XOr(x, Const(true))                    => stage(Not(x))(here).s  // Boolean simplification (x != true)
  }
  rewrite[XNor] {
    case XNor(Const(x:Boolean),Const(y:Boolean)) => bool(x == y)           // Constant propagation
    case XNor(Const(false), y)                   => stage(Not(y))(here).s  // Boolean simplification (false == y)
    case XNor(Const(true), y)                    => y                      // Boolean simplification (true == y)
    case XNor(x, Const(false))                   => stage(Not(x))(here).s  // Boolean simplification (x == false)
    case XNor(x, Const(true))                    => x                      // Boolean simplification (x == true)
  }

  /** Evaluation **/
  //eval[RandomBool]{ case _ => scala.util.Random.nextBoolean() }

  /** Internal methods **/
  implicit def lift(x: Boolean): Bool = const[Bool](x)
  def bool(x: Boolean): Sym[Bool] = const[Bool](x).s
}
