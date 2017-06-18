package argon.lang

import argon.core._
import argon.nodes._
import forge._


case class String(s: Exp[String]) extends MetaAny[String] {
  override type Internal = java.lang.String
  @api def +(rhs: CString): MString = this + String(rhs)
  @api def +(rhs: MString): MString = String(String.concat(this.toText.s, rhs.s))
  @api def +[R](rhs: MetaAny[R]): MString = String(String.concat(this.s, rhs.toText.s))

  @api def =!=(that: MString): MBoolean = Boolean(String.differ(this.s, that.s))
  @api def ===(that: MString): MBoolean = Boolean(String.equals(this.s, that.s))
  @api def equals(that: MString): MBoolean = Boolean(String.equals(this.s, that.s))
  @api def toText = this

  @api def length: Int32 = wrap(String.length(this.s))
  @api def apply(id: Index): MString = wrap(String.slice(this.s, id.s, (id+1).s))
  @api def apply(start: Index, end: Index): MString = wrap(String.slice(this.s, start.s, end.s))
}

object String {
  @api def apply(s: CString): MString = String(const(s))
  @internal def const(s: CString): Const[MString] = constant(StringType)(s)

  @internal def ify[T:Type](x: T): MString = String(sym_tostring(x.s))

  /** Type classes **/
  implicit def stringIsStaged: Type[String] = StringType

  /** Rewrite Rules **/
  /*@rewrite def MBoolean$not(x: Exp[Bool])(implicit ctx: SrcCtx): Exp[Bool] = x match {
    case Op(TextEquals(a,b)) => text_differ(a,b)
    case Op(TextDiffer(a,b)) => text_equals(a,b)
  }*/

  /** Constructors **/
  @internal def sym_tostring[T:Type](x: Exp[T]): Exp[MString] = x match {
    case Const(c: CString) => String.const(c)
    case a if a.tp == StringType => a.asInstanceOf[Exp[MString]]
    case _ => stage(ToString(x))(ctx)
  }
  @internal def concat(x: Exp[MString], y: Exp[MString]): Exp[MString] = (x,y) match {
    case (Const(a: CString), Const(b: CString)) => String.const(a + b)
    case (Const(""), b) => b
    case (a, Const("")) => a
    case _ => stage( StringConcat(x,y) )(ctx)
  }
  @internal def equals(x: Exp[MString], y: Exp[MString]): Exp[MBoolean] = (x,y) match {
    case (Const(a: CString), Const(b: CString)) => Boolean.const(a == b)
    case _ => stage( StringEquals(x,y) )(ctx)
  }
  @internal def differ(x: Exp[MString], y: Exp[MString]): Exp[MBoolean] = (x,y) match {
    case (Const(a: CString), Const(b: CString)) => Boolean.const(a != b)
    case _ => stage( StringDiffer(x,y) )(ctx)
  }

  @internal def slice(x: Exp[MString], start: Exp[Index], end: Exp[Index])(implicit ctx: SrcCtx): Exp[MString] = {
    stage( StringSlice(x,start,end) )(ctx)
  }
  @internal def length(x: Exp[MString])(implicit ctx: SrcCtx): Exp[Int32] = {
    stage( StringLength(x) )(ctx)
  }
}
