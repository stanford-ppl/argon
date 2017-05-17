package argon.lang

import argon._
import argon.core.NoFieldException
import argon.nodes._
import forge._

/**
  * Parent class for all staged Structs
  */
abstract class Struct[T:StructType] extends MetaAny[T]{ self =>
  protected def tp = implicitly[StructType[T]]
  @internal protected def field[F:Type](name: CString)(implicit ctx: SrcCtx, state: State): F = wrap{
    Struct.field_apply[T,F](self.s, name)
  }
  @internal protected def fieldToText[F](name: CString, tp: Type[F])(implicit ctx: SrcCtx, state: State): MString = {
    implicit val mF: Type[F] = tp
    tp.ev(field[F](name)).toText
  }

  @internal def fields(implicit ctx: SrcCtx, state: State): Seq[(CString,MetaAny[_])] = {
    tp.fields.map{case (name,fieldTyp) =>
      name -> fieldTyp.ev(field(name)(mtyp(fieldTyp),ctx,state))
    }
  }

  @api def =!=(that: T): MBoolean = Struct.unequals(this.asInstanceOf[T],that)
  @api def ===(that: T): MBoolean = Struct.equals(this.asInstanceOf[T],that)
  @api def toText = {
    val fields = tp.fields.map{case (name,fieldTyp) => fieldToText(name,fieldTyp) }
    MString(tp.prefix + "(") + fields.reduceLeft{(a,b) => a + "," + b } + ")"
  }
}



object Struct {
  /** Static methods **/
  def unapply(x: Op[_]): Option[Map[CString,Exp[_]]] = x match {
    case s: StructAlloc[_] => Some(s.elems.toMap)
    case _ => None
  }

  @internal def struct[T:StructType](fields: (CString, Exp[_])*): T = wrap(struct_new[T](fields))
  @internal def field[T:StructType,R](struct: T, name: CString, typ: Type[R]): R = {
    implicit val mR: Type[R] = typ
    typ.wrapped(field_apply[T,R](struct.s, name))
  }

  @internal private[argon] def equals[T:StructType](a: T, b: T): MBoolean = {
    val tp = implicitly[StructType[T]]
    def eql[F:Type](x: F, y: F): MBoolean = x === y

    tp.fields.map {case (name, fieldTyp) =>
      implicit val mA: Type[_] = fieldTyp
      val x = field(a, name, mA)
      val y = field(b, name, mA)
      eql(x,y)(mtyp(fieldTyp))
    }.reduce(_&&_)
  }
  @internal private[argon] def unequals[T:StructType](a: T, b: T): MBoolean = {
    val tp = implicitly[StructType[T]]
    def neq[F:Type](x: F, y: F): MBoolean = x =!= y

    tp.fields.map {case (name, fieldTyp) =>
      implicit val mA: Type[_] = fieldTyp
      val x = field(a, name, mA)
      val y = field(b, name, mA)
      neq(x,y)(mtyp(fieldTyp))
    }.reduce(_||_)
  }

  @stateful private[argon] def unwrapStruct[S:StructType,T:Type](struct: Exp[S], index: CString): Option[Exp[T]] = struct match {
    case Op(Struct(elems)) => elems.get(index) match {
      case Some(x) if x.tp <:< typ[T] => Some(x.asInstanceOf[Exp[T]]) // TODO: Should this be Staged asInstanceOf?
      case None =>
        throw new NoFieldException(struct, index) // TODO: Should this be a user error?
    }
    case _ => None
  }


  /** Constructors **/
  @internal def struct_new[S:StructType](elems: Seq[(CString, Exp[_])]): Exp[S] = {
    stage(SimpleStruct(elems))(ctx)
  }

  // TODO: Should struct unwrapping be disabled for mutable structs?
  @internal def field_apply[S:StructType,T:Type](struct: Exp[S], index: CString): Exp[T] = struct match {
    case Op(s:StructAlloc[_]) if Config.unwrapStructs => unwrapStruct[S,T](struct, index) match {
      case Some(x) => x
      case None => stage(FieldApply[S,T](struct, index))(ctx)
    }
    case _ => stage(FieldApply[S,T](struct, index))(ctx)
  }
  @internal def field_update[S:StructType,T:Type](struct: Exp[S], index: CString, value: Exp[T]): Exp[MUnit] = {
    stageWrite(struct)(FieldUpdate(struct, index, value))(ctx)
  }
}


