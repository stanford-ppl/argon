package argon.lang

import argon.core._
import argon.nodes._
import forge._

case class Var[T:Type](s: Exp[Var[T]])(implicit selfType: Type[Var[T]]) extends MetaAny[Var[T]] {
  @internal protected def read = wrap{ Var.read_var(this.s) }

  @api override def ===(that: Var[T]): MBoolean = this.read === that.read
  @api override def =!=(that: Var[T]): MBoolean = this.read =!= that.read
  @api override def toText: MString = this.read.toText
}

object Var {
  @internal def new_var[T:Type](init: Exp[T]): Exp[Var[T]] = {
    implicit val tp: Type[Var[T]] = VarType(typ[T])
    stageMutable(NewVar(init))(ctx)
  }
  @internal def read_var[T:Type](v: Exp[Var[T]]): Exp[T] = stage(ReadVar(v))(ctx)
  @internal def assign_var[T:Type](v: Exp[Var[T]], x: Exp[T]): Exp[MUnit] = stageWrite(v)(AssignVar(v, x))(ctx)
}
