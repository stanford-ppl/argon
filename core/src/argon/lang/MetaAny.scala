package argon.lang

import argon.core._
import forge._

/** Base trait for all staged, frontend types **/
abstract class MetaAny[T:Type] extends Product {
  type Internal
  def s: Exp[T]
  final def getOrElseCreate(func: => T): T = if (s == null) func else this.asInstanceOf[T]

  private def isEqual(that: Any): CBoolean = that match {
    case x: MetaAny[_] => this.s == x.s
    case _ => false
  }

  /*private def unstagedWarning(op: String)(implicit ctx: SrcCtx): Unit = {
    warn(s"Unstaged method $op was used here on a staged type during staging.")
    warn("Add @virtualize annotation to an enclosing scope to prevent this.")
    warn(ctx)
  }*/
  // TODO: Some way of doing this while also having context?
  private def unstagedWarningNoCtx(op: CString): CUnit = {
    val name = s.name.getOrElse(s.toString)
    //Report.warn(s"Unstaged method $op was used on value $name during staging.")
    //Report.warn("Add @virtualize annotation to an enclosing scope to prevent this.")
  }

  override def toString: CString = {
    unstagedWarningNoCtx("toString")
    this.productPrefix + this.productIterator.mkString("(", ", ", ")")
  }

  override def equals(that: Any): CBoolean = {
    unstagedWarningNoCtx("equals")
    this.isEqual(that)
  }

  @api def !=(that: T): MBoolean = this =!= that
  @api def ==(that: T): MBoolean = this === that
  @api def ===(that: T): MBoolean
  @api def =!=(that: T): MBoolean
  @api def toText: MString
}

object Literal {
  def unapply[T<:MetaAny[T]](s: Exp[T]): Option[T#Internal] = s match {
    case param: Param[_] if param.isFinal => Some(param.c.asInstanceOf[T#Internal])
    case const: Const[_] => Some(const.c.asInstanceOf[T#Internal])
    case _ => None
  }
}
