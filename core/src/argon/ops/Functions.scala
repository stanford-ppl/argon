package argon.ops

import argon._
import forge._
import org.virtualized.EmptyContext

trait FunctionApi extends FunctionExp {
  self: ArgonApi =>

  type ArgonFunction1[T, R] <: T => R
}

trait FunctionExp {
  self: ArgonExp =>

  case class Apply[T:Type, R:Type](fun: Exp[ArgonFunction1[T,R]], arg: Exp[T]) extends Op[R] {
    def mirror(f: Tx): Exp[R] = fun_apply(f(fun), f(arg))
  }

  def fun_apply[T:Type, R:Type](f: Exp[ArgonFunction1[T,R]], arg: Exp[T])(implicit ctx: SrcCtx): Exp[R] = stage(Apply(f, arg))(ctx)

  case class ArgonFunction1[T:Type, R:Type](s: Exp[ArgonFunction1[T, R]]) extends MetaAny[ArgonFunction1[T,R]] with (T => R) {
    @api def ===(that: ArgonFunction1[T, R]) = ???
    @api def =!=(that: ArgonFunction1[T, R]) = ???
    @api def toText = textify(this)

    def apply(x: T): R = applyArg(x)(EmptyContext)
    @api def applyArg(x: T): R = wrap(fun_apply(s, x.s))
  }

  @internal def fun[T:Type, R:Type](f: T => R): ArgonFunction1[T,R] = {
    val arg1 = fresh[T]
    val warg1:T = wrap(arg1)
    val body: R = f(warg1)
    ???
  }

  /** Type classes **/
  case class ArgonFunction1Type[T, R](childT: Meta[T], childR: Meta[R]) extends Meta[ArgonFunction1[T, R]] {
    override def wrapped(x: Exp[ArgonFunction1[T,R]]) = ArgonFunction1(x)(childT, childR)
    override def unwrapped(x: ArgonFunction1[T,R]) = x.s
    override def stagedClass = classOf[ArgonFunction1[T,R]]
    override def typeArguments = List(childT, childR)
    override def isPrimitive: Boolean = false
  }
  implicit def argonFunction1Type[T:Meta, R:Meta]: ArgonFunction1Type[T,R] = ArgonFunction1Type[T,R](meta[T], meta[R])



}
