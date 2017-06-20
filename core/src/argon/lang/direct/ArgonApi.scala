package argon.lang.direct

import typeclasses._

import argon.lang.cake.ArgonLangAliases
import forge._

/** Implicit typeclass evidence, (optionally) implicit conversions **/
trait ArgonExp extends ArgonLangAliases
  with BooleanExp
  with FixPtExp
  with FltPtExp
  with FunctionExp
  with IfThenElseExp
  with MetaAnyExp
  with OverloadExp
  with StringExp
  with StructExp
  with UnitExp
  with VarExp
  with ArithExp
  with BitsExp
  with NumExp
  with OrderExp
  with EqualMacros

trait ArgonApi extends ArgonExp
  with ArrayApi
  with CastsApi
  with HashMapApi
  with BitsApi
  with NumApi
  with Tuple2Api
{

  class ArithOps[T:Arith](lhs: T) {
    @api def unary_-(): T = arith[T].negate(lhs)
    @api def +(rhs: T): T = arith[T].plus(lhs, rhs)
    @api def -(rhs: T): T = arith[T].minus(lhs, rhs)
    @api def *(rhs: T): T = arith[T].times(lhs, rhs)
    @api def /(rhs: T): T = arith[T].divide(lhs, rhs)
  }
  implicit def arithOps[T:Arith](lhs: T): ArithOps[T] = new ArithOps[T](lhs)
}
