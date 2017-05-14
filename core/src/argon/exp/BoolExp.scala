package argon.exp

import argon._
import forge._

case class Bool(s: Exp[Bool]) extends MetaAny[Bool] {
  @api def unary_!(): Bool = wrap{ BoolExp.not(this.s) }
  @api def &&(that: Bool): Bool = wrap { BoolExp.and(this.s, that.s) }
  @api def ||(that: Bool): Bool = wrap { BoolExp.or(this.s, that.s) }
  @api def ^ (that: Bool): Bool = wrap { BoolExp.xor(this.s, that.s) }

  @api def ===(that: Bool): Bool = wrap{ BoolExp.xnor(this.s, that.s) }
  @api def =!=(that: Bool): Bool = wrap{ BoolExp.xor(this.s, that.s) }
  @api def toText: Text = textify(this)
}


/** IR Nodes **/
case class Not(a: Exp[Bool]) extends Op[Bool] { def mirror(f:Tx) = BoolExp.not(f(a)) }
case class And(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool]  { def mirror(f:Tx) = BoolExp.and(f(a), f(b)) }
case class Or(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool]   { def mirror(f:Tx) = BoolExp.or(f(a), f(b)) }
case class XOr(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool]  { def mirror(f:Tx) = BoolExp.xor(f(a), f(b)) }
case class XNor(a: Exp[Bool], b: Exp[Bool]) extends Op[Bool] { def mirror(f:Tx) = BoolExp.xnor(f(a), f(b)) }
case class RandomBool(max: Option[Exp[Bool]]) extends Op[Bool] { def mirror(f:Tx) = BoolExp.random(f(max)) }
case class StringToBool(x: Exp[Text]) extends Op[Bool] { def mirror(f:Tx) = BoolExp.fromText(x) }


object BoolExp {
  @internal def lift(x: Boolean): Bool = Bool(bool(x))
  @internal def bool(x: Boolean): Exp[Bool] = constant[Bool](x)

  /** Constructors **/
  @internal def not(x: Exp[Bool]): Exp[Bool] = x match {
    case Const(c: Boolean) => bool(!c)    // Constant propagation
    case Op(Not(a)) => a                  // Boolean simplification: !(!a) => a
    case _ => stage(Not(x))(ctx)          // Default constructor
  }
  @internal def and(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean), Const(b:Boolean)) => bool(a && b)     // Constant propagation
    case (Const(false), _)                    => bool(false)      // Short circuit evaluation: false && _ => false
    case (_, Const(false))                    => bool(false)      // Short circuit evaluation: _ && false => false
    case (Const(true), b)                     => b                // Boolean simplification: true && b => b
    case (a, Const(true))                     => a                // Boolean simplification: a && true => a
    case _ => stage( And(x,y) )(ctx)                              // Default constructor
  }
  @internal def or(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => bool(a || b)       // Constant propagation
    case (Const(true), _)                    => bool(true)         // Short circuit evaluation: true || _ => true
    case (_, Const(true))                    => bool(true)         // Short circuit evaluation: _ || true => true
    case (Const(false), b)                   => b                  // Boolean simplification: false || b => b
    case (a, Const(false))                   => a                  // Boolean simplification: a || false => a
    case _ => stage( Or(x,y) )(ctx)                                // Default constructor
  }
  @internal def xor(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => bool(a != b)           // Constant propagation
    case (Const(false), b)                   => b                      // Boolean simplification: false != b => b
    case (Const(true), b)                    => stage(Not(b))(ctx)     // Boolean simplification: true != b => !b
    case (a, Const(false))                   => a                      // Boolean simplification: a != false => a
    case (a, Const(true))                    => stage(Not(a))(ctx)     // Boolean simplification: a != true => !a
    case _ => stage( XOr(x,y) )(ctx)                                   // Default constructor
  }
  @internal def xnor(x: Exp[Bool], y: Exp[Bool]): Exp[Bool] = (x,y) match {
    case (Const(a:Boolean),Const(b:Boolean)) => bool(a == b)           // Constant propagation
    case (Const(false), b)                   => stage(Not(b))(ctx)     // Boolean simplification: false == b => !b
    case (Const(true), b)                    => b                      // Boolean simplification: true == b => b
    case (a, Const(false))                   => stage(Not(a))(ctx)     // Boolean simplification: a == false => !a
    case (a, Const(true))                    => a                      // Boolean simplification: a == true => a
    case _ => stage( XNor(x,y) )(ctx)                                  // Default constructor
  }
  @internal def random(max: Option[Exp[Bool]]): Exp[Bool] = max match {
    case Some(Const(false)) => bool(false)
    case _ => stageSimple(RandomBool(max))(ctx)
  }
  @internal def fromText(x: Exp[Text]): Exp[Bool] = x match {
    case Const("true") => bool(true)
    case Const("false") => bool(false)
    case _ => stage(StringToBool(x))(ctx)
  }
}
