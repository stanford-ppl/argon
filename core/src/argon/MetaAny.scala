package argon

import argon._
import forge._
import argon.exp.{Text, Bool}


/** Base trait for all staged, frontend types **/
abstract class MetaAny[T:Type] extends Product {
  def s: Exp[T]

  private def isEqual(that: Any): Boolean = that match {
    case x: MetaAny[_] => this.s == x.s
    case _ => false
  }

  @internal def unstagedWarning(op: String): Unit = {
    warn(ctx, s"Unstaged method $op was used here on a staged type during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }
  @internal def unstagedWarningNoCtx(op: String): Unit = {
    val name = ctx.lhsName.getOrElse("the value")
    warn(ctx, s"Unstaged method $op was used on $name defined here during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }

  override def toString(): String = {
    if (State.staging) unstagedWarningNoCtx("toString()")(s.ctx)
    this.productPrefix + this.productIterator.mkString("(", ", ", ")")
  }

  override def equals(that: Any): Boolean = {
    if (State.staging) unstagedWarningNoCtx("equals")(s.ctx)
    this.isEqual(that)
  }

  @api def !=(that: T): Bool = this =!= that
  @api def ==(that: T): Bool = this === that
  @api def ===(that: T): Bool
  @api def =!=(that: T): Bool
  @api def toText: Text
}