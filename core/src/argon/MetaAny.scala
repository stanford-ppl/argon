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

  /*private def unstagedWarning(op: String)(implicit ctx: SrcCtx): Unit = {
    warn(s"Unstaged method $op was used here on a staged type during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }*/
  private def unstagedWarningNoCtx(op: String): Unit = {
    val name = s.name.getOrElse(s.toString)
    warn(s"Unstaged method $op was used on value $name during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
  }

  override def toString: String = {
    if (Globals.staging) unstagedWarningNoCtx("toString")
    this.productPrefix + this.productIterator.mkString("(", ", ", ")")
  }

  override def equals(that: Any): Boolean = {
    if (Globals.staging) unstagedWarningNoCtx("equals")
    this.isEqual(that)
  }

  @api def !=(that: T): MBoolean = this =!= that
  @api def ==(that: T): MBoolean = this === that
  @api def ===(that: T): MBoolean
  @api def =!=(that: T): MBoolean
  @api def toText: MString
}