package argon.ops
import argon.core.Staging
import argon.typeclasses._

trait BoolApi extends BoolExp with BitsApi { this: TextApi =>
  type Boolean = Bool
}

trait BoolExp extends Staging with BitsExp { this: TextExp =>
  /** Type classes **/
  // --- Staged
  implicit object BoolType extends FStaged[Bool] {
    override def wrapped(x: Exp[Bool]): Bool = Bool(x)
    override def typeArguments = Nil
    override def stagedClass = classOf[Bool]
    override def isPrimitive = true
  }

  /** Infix methods **/
  case class Bool(s: Exp[Bool]) extends StageAny[Bool] {
    def unary_!(implicit ctx: SrcCtx): Bool = Bool(bool_not(this.s)(ctx))
    def &&(that: Bool)(implicit ctx: SrcCtx): Bool = Bool(bool_and(this.s, that.s)(ctx))
    def ||(that: Bool)(implicit ctx: SrcCtx): Bool = Bool( bool_or(this.s, that.s)(ctx))
    def ^ (that: Bool)(implicit ctx: SrcCtx): Bool = Bool(bool_xor(this.s, that.s)(ctx))

    def +[A](rhs: A)(implicit ctx: SrcCtx, lft: Lift[A,Text]): Text = textify(this) + lft.lift(rhs)
  }

  /** Virtualized methods **/
  def infix_==(x: Bool, y: Bool)(implicit ctx: SrcCtx): Bool = Bool(bool_xnor(x.s, y.s)(ctx))
  def infix_!=(x: Bool, y: Bool)(implicit ctx: SrcCtx): Bool = Bool(bool_xor(x.s, y.s)(ctx))
  def infix_toString(x: Bool)(implicit ctx: SrcCtx): Text = textify(x)


  // --- Bits
  implicit object BoolBits extends Bits[Bool] {
    override def zero(implicit ctx: SrcCtx): Bool = boolean2bool(false)
    override def one(implicit ctx: SrcCtx): Bool = boolean2bool(true)
    override def random(max: Option[Bool])(implicit ctx: SrcCtx): Bool = Bool(bool_random(max.map(_.s)))
    override def length = 1
  }
  override protected def bitsUnapply[T <: StageAny[T]](tp: Staged[T]): Option[Bits[T]] = tp match {
    case BoolType => Some(BoolBits.asInstanceOf[Bits[T]])
    case _ => super.bitsUnapply(tp)
  }

  // --- Lifts
  implicit object Boolean2Bool extends Lift[Boolean,Bool] { val Staged = BoolType }


  /** Constant lifting **/
  implicit def boolean2bool(x: Boolean)(implicit ctx: SrcCtx): Bool = lift(x)
  def bool(x: Boolean)(implicit ctx: SrcCtx): Const[Bool] = constant[Bool](x)


  /** IR Nodes **/
  case class Not(a: Exp[Bool]) extends Op[Bool] { def mirror(f:Tx) = bool_not(f(a)) }
  case class And(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool]  { def mirror(f:Tx) = bool_and(f(a), f(b)) }
  case class Or(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool]   { def mirror(f:Tx) = bool_or(f(a), f(b)) }
  case class XOr(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool]  { def mirror(f:Tx) = bool_xor(f(a), f(b)) }
  case class XNor(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool] { def mirror(f:Tx) = bool_xnor(f(a), f(b)) }
  case class RandomBool(max: Option[Exp[Bool]]) extends Op[Bool] { def mirror(f:Tx) = bool_random(f(max)) }


  /** Constructors **/
  def bool_not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Const(c: Boolean) => bool(!c)    // Constant propagation
    case Op(Not(a)) => a                  // Boolean simplification: !(!a) => a
    case _ => stage(Not(x))(ctx)          // Default constructor
  }
  def bool_and(x: Exp[Bool], y: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean), Const(b:Boolean)) => bool(a && b)     // Constant propagation
    case (Const(false), _)                    => bool(false)      // Short circuit evaluation: false && _ => false
    case (_, Const(false))                    => bool(false)      // Short circuit evaluation: _ && false => false
    case (Const(true), b)                     => b                // Boolean simplification: true && b => b
    case (a, Const(true))                     => a                // Boolean simplification: a && true => a
    case _ => stage( And(x,y) )(ctx)                              // Default constructor
  }
  def bool_or(x: Exp[Bool], y: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => bool(a || b)       // Constant propagation
    case (Const(true), _)                    => bool(true)         // Short circuit evaluation: true || _ => true
    case (_, Const(true))                    => bool(true)         // Short circuit evaluation: _ || true => true
    case (Const(false), b)                   => b                  // Boolean simplification: false || b => b
    case (a, Const(false))                   => a                  // Boolean simplification: a || false => a
    case _ => stage( Or(x,y) )(ctx)                                // Default constructor
  }
  def bool_xor(x: Exp[Bool], y: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => bool(a != b)           // Constant propagation
    case (Const(false), b)                   => b                      // Boolean simplification: false != b => b
    case (Const(true), b)                    => stage(Not(b))(ctx)     // Boolean simplification: true != b => !b
    case (a, Const(false))                   => a                      // Boolean simplification: a != false => a
    case (a, Const(true))                    => stage(Not(a))(ctx)     // Boolean simplification: a != true => !a
    case _ => stage( XOr(x,y) )(ctx)                                   // Default constructor
  }
  def bool_xnor(x: Exp[Bool], y: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => bool(a == b)           // Constant propagation
    case (Const(false), b)                   => stage(Not(b))(ctx)     // Boolean simplification: false == b => !b
    case (Const(true), b)                    => b                      // Boolean simplification: true == b => b
    case (a, Const(false))                   => stage(Not(a))(ctx)     // Boolean simplification: a == false => !a
    case (a, Const(true))                    => a                      // Boolean simplification: a == true => a
    case _ => stage( XNor(x,y) )(ctx)                                  // Default constructor
  }
  def bool_random(max: Option[Exp[Bool]])(implicit ctx: SrcCtx): Exp[Bool] = max match {
    case Some(Const(false)) => bool(false)
    case _ => stageSimple(RandomBool(max))(ctx)
  }
}
