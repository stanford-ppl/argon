package argon

import argon._
import forge._

/** Base trait for all staged, frontend types **/
abstract class MetaAny[T:Type] extends Product {
  type Internal = Any
  def s: Exp[T]

  private def isEqual(that: Any): Boolean = that match {
    case x: MetaAny[_] => this.s == x.s
    case _ => false
  }

  private def unstagedWarning(op: String)(implicit ctx: SrcCtx): Unit = {
    warn(s"$ctx: Unstaged method $op was used here on a staged type during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }
  private def unstagedWarningNoCtx(op: String)(implicit ctx: SrcCtx): Unit = {
    val name = ctx.lhsName.getOrElse("the value")
    warn(s"$ctx: Unstaged method $op was used on $name defined here during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }

  override def toString: String = {
    if (Globals.staging) unstagedWarningNoCtx("toString")(s.ctx)
    this.productPrefix + this.productIterator.mkString("(", ", ", ")")
  }

  override def equals(that: Any): Boolean = {
    if (Globals.staging) unstagedWarningNoCtx("equals")(s.ctx)
    this.isEqual(that)
  }

  @api def !=(that: T): MBoolean = this =!= that
  @api def ==(that: T): MBoolean = this === that
  @api def ===(that: T): MBoolean
  @api def =!=(that: T): MBoolean
  @api def toText: MString
}