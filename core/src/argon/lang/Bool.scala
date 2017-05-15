package argon.lang

import argon._
import argon.typeclasses._
import forge._

case class Bool(s: Exp[Bool]) extends MetaAny[Bool] {
  protected val bool = Bool
  @api def unary_!(): Bool = Bool(bool.not(this.s))
  @api def &&(that: Bool): Bool = Bool(bool.and(this.s, that.s))
  @api def ||(that: Bool): Bool = Bool(bool.or(this.s, that.s))
  @api def ^ (that: Bool): Bool = Bool(bool.xor(this.s, that.s))

  @api def ===(that: Bool): Bool = Bool(bool.xnor(this.s, that.s))
  @api def =!=(that: Bool): Bool = Bool(bool.xor(this.s, that.s))
  @api def toText: Text = Text.ify(this)
}

case object BoolType extends Type[Bool] with CanBits[Bool] {
  override def wrapped(x: Exp[Bool]): Bool = Bool(x)
  override def typeArguments = Nil
  override def stagedClass = classOf[Bool]
  override def isPrimitive = true
  protected def getBits(children: Seq[Type[_]]) = Some(BoolBits)

  def unapply(x: Type[_]): Boolean = x == BoolType
}
object BoolBits extends Bits[Bool] {
  @internal def zero: Bool = Bool(false)
  @internal def one: Bool = Bool(true)
  @internal def random(max: Option[Bool]): Bool = Bool(Bool.random(max.map(_.s)))
  @internal def length = 1
}


object Bool {
  @api def apply(x: Boolean): Bool = Bool(const(x))
  @internal def lift(x: Boolean): Bool = Bool(const(x))
  @internal def const(x: Boolean): Exp[Bool] = constant[Bool](x)

  /** Constructors **/
  @internal def not(x: Exp[Bool]): Exp[Bool] = x match {
    case Const(c: Boolean) => const(!c)   // Constant propagation
    case Op(Not(a)) => a                  // Boolean simplification: !(!a) => a
    case _ => stage(Not(x))(ctx)          // Default constructor
  }
  @internal def and(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean), Const(b:Boolean)) => const(a && b)    // Constant propagation
    case (Const(false), _)                    => const(false)     // Short circuit evaluation: false && _ => false
    case (_, Const(false))                    => const(false)     // Short circuit evaluation: _ && false => false
    case (Const(true), b)                     => b                // Boolean simplification: true && b => b
    case (a, Const(true))                     => a                // Boolean simplification: a && true => a
    case _ => stage( And(x,y) )(ctx)                              // Default constructor
  }
  @internal def or(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => const(a || b)      // Constant propagation
    case (Const(true), _)                    => const(true)        // Short circuit evaluation: true || _ => true
    case (_, Const(true))                    => const(true)        // Short circuit evaluation: _ || true => true
    case (Const(false), b)                   => b                  // Boolean simplification: false || b => b
    case (a, Const(false))                   => a                  // Boolean simplification: a || false => a
    case _ => stage( Or(x,y) )(ctx)                                // Default constructor
  }
  @internal def xor(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => const(a != b)          // Constant propagation
    case (Const(false), b)                   => b                      // Boolean simplification: false != b => b
    case (Const(true), b)                    => stage(Not(b))(ctx)     // Boolean simplification: true != b => !b
    case (a, Const(false))                   => a                      // Boolean simplification: a != false => a
    case (a, Const(true))                    => stage(Not(a))(ctx)     // Boolean simplification: a != true => !a
    case _ => stage( XOr(x,y) )(ctx)                                   // Default constructor
  }
  @internal def xnor(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => const(a == b)          // Constant propagation
    case (Const(false), b)                   => stage(Not(b))(ctx)     // Boolean simplification: false == b => !b
    case (Const(true), b)                    => b                      // Boolean simplification: true == b => b
    case (a, Const(false))                   => stage(Not(a))(ctx)     // Boolean simplification: a == false => !a
    case (a, Const(true))                    => a                      // Boolean simplification: a == true => a
    case _ => stage( XNor(x,y) )(ctx)                                  // Default constructor
  }
  @internal def random(max: Option[Exp[Bool]]): Exp[Bool] = max match {
    case Some(Const(false)) => const(false)
    case _ => stageSimple(RandomBool(max))(ctx)
  }
  @internal def from_text(x: Exp[Text]): Exp[Bool] = x match {
    case Const("true") => const(true)
    case Const("false") => const(false)
    case _ => stage(StringToBool(x))(ctx)
  }
}

trait BoolExp {
  /** Type **/
  implicit def boolIsStaged: Type[Bool] = BoolType
  implicit def boolIsBits: Bits[Bool] = BoolBits

  /** Lifting **/
  @api implicit def boolean2bool(x: Boolean): Bool = Bool(x)

  implicit object LiftBoolean2Bool extends Lift[Boolean,Bool] {
    @internal def apply(x: Boolean): Bool = Bool(x)
  }
  /** Casting **/
  implicit object CastBoolean2Bool extends Cast[Boolean,Bool] {
    @internal def apply(x: Boolean): Bool = Bool(x)
  }
  implicit object Text2Bool extends Cast[Text,Bool] {
    @internal def apply(x: Text): Bool = Bool(Bool.from_text(x.s))
  }
}


/** IR Nodes **/
sealed abstract class BoolOp extends Op[Bool] { protected val bool = Bool }

case class Not (a: Exp[Bool]) extends BoolOp { def mirror(f:Tx) = bool.not(f(a)) }
case class And (a: Exp[Bool], b: Exp[Bool]) extends BoolOp { def mirror(f:Tx) = bool.and(f(a), f(b)) }
case class Or  (a: Exp[Bool], b: Exp[Bool]) extends BoolOp { def mirror(f:Tx) = bool.or(f(a), f(b)) }
case class XOr (a: Exp[Bool], b: Exp[Bool]) extends BoolOp { def mirror(f:Tx) = bool.xor(f(a), f(b)) }
case class XNor(a: Exp[Bool], b: Exp[Bool]) extends BoolOp { def mirror(f:Tx) = bool.xnor(f(a), f(b)) }
case class RandomBool(max: Option[Exp[Bool]]) extends BoolOp { def mirror(f:Tx) = bool.random(f(max)) }
case class StringToBool(x: Exp[Text]) extends BoolOp { def mirror(f:Tx) = bool.from_text(x) }

