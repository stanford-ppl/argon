package argon.lang

import typeclasses.Bits
import argon.internals._
import argon.nodes._
import forge._

case class Boolean(s: Exp[Boolean]) extends MetaAny[Boolean] {
  override type Internal = scala.Boolean
  protected val bool = Boolean
  @api def unary_!(): MBoolean = Boolean(bool.not(this.s))
  @api def &&(that: MBoolean): MBoolean = Boolean(bool.and(this.s, that.s))
  @api def ||(that: MBoolean): MBoolean = Boolean(bool.or(this.s, that.s))
  @api def ^ (that: MBoolean): MBoolean = Boolean(bool.xor(this.s, that.s))

  @api def ===(that: MBoolean): MBoolean = Boolean(bool.xnor(this.s, that.s))
  @api def =!=(that: MBoolean): MBoolean = Boolean(bool.xor(this.s, that.s))
  @api def toText: MString = String.ify(this)
}

object Boolean {
  /** Static functions **/
  @api def apply(x: CBoolean): MBoolean = Boolean(const(x))
  @internal def lift(x: CBoolean): MBoolean = Boolean(const(x))
  @internal def const(x: CBoolean): Exp[MBoolean] = constant(BooleanType)(x)

  /** Type **/
  implicit def boolIsStaged: Type[MBoolean] = BooleanType
  implicit def boolIsBits: Bits[MBoolean] = BooleanBits

  @api implicit def boolean2bool(x: CBoolean): MBoolean = Boolean(x)

  /** Constructors **/
  @internal def not(x: Exp[MBoolean]): Exp[MBoolean] = x match {
    case Const(c: CBoolean) => const(!c)  // Constant propagation
    case Op(Not(a)) => a                  // Boolean simplification: !(!a) => a
    case _ => stage(Not(x))(ctx)          // Default constructor
  }
  @internal def and(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Const(a: CBoolean), Const(b: CBoolean)) => const(a && b)  // Constant propagation
    case (Const(false), _)    => const(false)                       // Short circuit evaluation: false && _ => false
    case (_, Const(false))    => const(false)                       // Short circuit evaluation: _ && false => false
    case (Const(true), b)     => b                                  // Boolean simplification: true && b => b
    case (a, Const(true))     => a                                  // Boolean simplification: a && true => a
    case _ => stage( And(x,y) )(ctx)                                // Default constructor
  }
  @internal def or(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Const(a: CBoolean),Const(b: CBoolean))   => const(a || b) // Constant propagation
    case (Const(true), _)    => const(true)                         // Short circuit evaluation: true || _ => true
    case (_, Const(true))    => const(true)                         // Short circuit evaluation: _ || true => true
    case (Const(false), b)   => b                                   // Boolean simplification: false || b => b
    case (a, Const(false))   => a                                   // Boolean simplification: a || false => a
    case _ => stage( Or(x,y) )(ctx)                                 // Default constructor
  }
  @internal def xor(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Const(a: CBoolean),Const(b: CBoolean))   => const(a != b) // Constant propagation
    case (Const(false), b)   => b                      // Boolean simplification: false != b => b
    case (Const(true), b)    => stage(Not(b))(ctx)     // Boolean simplification: true != b => !b
    case (a, Const(false))   => a                      // Boolean simplification: a != false => a
    case (a, Const(true))    => stage(Not(a))(ctx)     // Boolean simplification: a != true => !a
    case _ => stage( XOr(x,y) )(ctx)                   // Default constructor
  }
  @internal def xnor(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Const(a: CBoolean),Const(b: CBoolean))   => const(a == b) // Constant propagation
    case (Const(false), b)   => stage(Not(b))(ctx)     // Boolean simplification: false == b => !b
    case (Const(true), b)    => b                      // Boolean simplification: true == b => b
    case (a, Const(false))   => stage(Not(a))(ctx)     // Boolean simplification: a == false => !a
    case (a, Const(true))    => a                      // Boolean simplification: a == true => a
    case _ => stage( XNor(x,y) )(ctx)                  // Default constructor
  }
  @internal def random(max: Option[Exp[MBoolean]]): Exp[MBoolean] = max match {
    case Some(Const(false)) => const(false)
    case _ => stageSimple(RandomBoolean(max))(ctx)
  }
  @internal def from_string(x: Exp[MString]): Exp[MBoolean] = x match {
    case Const("true")  => const(true)
    case Const("false") => const(false)
    case _ => stage(StringToBoolean(x))(ctx)
  }
}

trait BooleanExp {
  /** Lifting **/
  implicit object LiftBoolean2Bool extends Lift[CBoolean,MBoolean] {
    @internal def apply(x: CBoolean): MBoolean = Boolean(x)
  }
  /** Casting **/
  implicit object CastBoolean2Bool extends Cast[CBoolean,MBoolean] {
    @internal def apply(x: CBoolean): MBoolean = Boolean(x)
  }
  implicit object String2Boolean extends Cast[MString,MBoolean] {
    @internal def apply(x: MString): MBoolean = Boolean(Boolean.from_string(x.s))
  }
}


