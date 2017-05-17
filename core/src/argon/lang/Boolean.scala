package argon.lang

import argon._
import argon.nodes._
import argon.typeclasses._
import forge._

case class Boolean(s: Exp[Boolean]) extends MetaAny[Boolean] {
  override type Internal = scala.Boolean
  protected val bool = Boolean
  @api def unary_!(): MBoolean = MBoolean(bool.not(this.s))
  @api def &&(that: MBoolean): MBoolean = MBoolean(bool.and(this.s, that.s))
  @api def ||(that: MBoolean): MBoolean = MBoolean(bool.or(this.s, that.s))
  @api def ^ (that: MBoolean): MBoolean = MBoolean(bool.xor(this.s, that.s))

  @api def ===(that: MBoolean): MBoolean = MBoolean(bool.xnor(this.s, that.s))
  @api def =!=(that: MBoolean): MBoolean = MBoolean(bool.xor(this.s, that.s))
  @api def toText: MString = String.ify(this)
}

object Boolean {
  @api def apply(x: CBoolean): MBoolean = MBoolean(const(x))
  @internal def lift(x: CBoolean): MBoolean = MBoolean(const(x))
  @internal def const(x: CBoolean): Exp[MBoolean] = constant(BooleanType)(x)

  /** Litructors **/
  @internal def not(x: Exp[MBoolean]): Exp[MBoolean] = x match {
    case Lit(c)     =>
      val d: MBoolean#Internal = c
      const(!d)          // Constant propagation
    case Op(Not(a)) => a                  // Boolean simplification: !(!a) => a
    case _ => stage(Not(x))(ctx)          // Default constructor
  }
  @internal def and(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Lit(a), Lit(b)) => const(a && b)      // Constant propagation
    case (Lit(false), _)    => const(false)     // Short circuit evaluation: false && _ => false
    case (_, Lit(false))    => const(false)     // Short circuit evaluation: _ && false => false
    case (Lit(true), b)     => b                // Boolean simplification: true && b => b
    case (a, Lit(true))     => a                // Boolean simplification: a && true => a
    case _ => stage( And(x,y) )(ctx)            // Default constructor
  }
  @internal def or(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Lit(a),Lit(b))   => const(a || b)      // Constant propagation
    case (Lit(true), _)    => const(true)        // Short circuit evaluation: true || _ => true
    case (_, Lit(true))    => const(true)        // Short circuit evaluation: _ || true => true
    case (Lit(false), b)   => b                  // Boolean simplification: false || b => b
    case (a, Lit(false))   => a                  // Boolean simplification: a || false => a
    case _ => stage( Or(x,y) )(ctx)              // Default constructor
  }
  @internal def xor(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Lit(a),Lit(b))   => const(a != b)          // Constant propagation
    case (Lit(false), b)   => b                      // Boolean simplification: false != b => b
    case (Lit(true), b)    => stage(Not(b))(ctx)     // Boolean simplification: true != b => !b
    case (a, Lit(false))   => a                      // Boolean simplification: a != false => a
    case (a, Lit(true))    => stage(Not(a))(ctx)     // Boolean simplification: a != true => !a
    case _ => stage( XOr(x,y) )(ctx)                 // Default constructor
  }
  @internal def xnor(x: Exp[MBoolean], y: Exp[MBoolean]): Exp[MBoolean] = (x,y) match {
    case (Lit(a),Lit(b))   => const(a == b)          // Constant propagation
    case (Lit(false), b)   => stage(Not(b))(ctx)     // Boolean simplification: false == b => !b
    case (Lit(true), b)    => b                      // Boolean simplification: true == b => b
    case (a, Lit(false))   => stage(Not(a))(ctx)     // Boolean simplification: a == false => !a
    case (a, Lit(true))    => a                      // Boolean simplification: a == true => a
    case _ => stage( XNor(x,y) )(ctx)                // Default constructor
  }
  @internal def random(max: Option[Exp[MBoolean]]): Exp[MBoolean] = max match {
    case Some(Lit(false)) => const(false)
    case _ => stageSimple(RandomBoolean(max))(ctx)
  }
  @internal def from_string(x: Exp[MString]): Exp[MBoolean] = x match {
    case Lit("true")  => const(true)
    case Lit("false") => const(false)
    case _ => stage(StringToBoolean(x))(ctx)
  }
}

trait BooleanExp {
  /** Type **/
  implicit def boolIsStaged: Type[MBoolean] = BooleanType
  implicit def boolIsBits: Bits[MBoolean] = BooleanBits

  /** Lifting **/
  @api implicit def boolean2bool(x: CBoolean): MBoolean = MBoolean(x)

  implicit object LiftBoolean2Bool extends Lift[CBoolean,MBoolean] {
    @internal def apply(x: CBoolean): MBoolean = MBoolean(x)
  }
  /** Casting **/
  implicit object CastBoolean2Bool extends Cast[CBoolean,MBoolean] {
    @internal def apply(x: CBoolean): MBoolean = MBoolean(x)
  }
  implicit object String2Boolean extends Cast[MString,MBoolean] {
    @internal def apply(x: MString): MBoolean = MBoolean(MBoolean.from_string(x.s))
  }
}


