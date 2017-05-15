package argon.lang

import argon._
import argon.core.NoFieldException
import forge._

/**
  * Parent class for all staged Structs
  */
abstract class Struct[T:StructType] extends MetaAny[T]{ self =>
  protected def tp = implicitly[StructType[T]]
  @internal protected def field[F:Type](name: String)(implicit ctx: SrcCtx, state: State): F = wrap{
    Struct.field_apply[T,F](self.s, name)
  }
  @internal protected def fieldToText[F](name: String, tp: Type[F])(implicit ctx: SrcCtx, state: State): Text = {
    tp.ev(field[F](name)).toText
  }

  @internal def fields(implicit ctx: SrcCtx, state: State): Seq[(String,MetaAny[_])] = {
    tp.fields.map{case (name,fieldTyp) =>
      name -> fieldTyp.ev(field(name)(mtyp(fieldTyp),ctx,state))
    }
  }

  @api def =!=(that: T): Bool = Struct.unequals(this.asInstanceOf[T],that)
  @api def ===(that: T): Bool = Struct.equals(this.asInstanceOf[T],that)
  @api def toText = {
    val fields = tp.fields.map{case (name,fieldTyp) => fieldToText(name,fieldTyp) }
    lift[String,Text](tp.prefix + "(") + fields.reduceLeft{(a,b) => a + "," + b } + ")"
  }
}

abstract class StructType[T](override implicit val ev: T <:< Struct[T]) extends Type[T] {
  override def isPrimitive = false
  def fields: Seq[(String, Type[_])]
  def prefix: String = this.stagedClass.getSimpleName
}

object Struct {
  /** Static methods **/
  def unapply(x: Op[_]): Option[Map[String,Exp[_]]] = x match {
    case s: StructAlloc[_] => Some(s.elems.toMap)
    case _ => None
  }

  @internal def struct[T:StructType](fields: (String, Exp[_])*): T = wrap(struct_new[T](fields))
  @internal def field[T:StructType,R](struct: T, name: String, typ: Type[R]): R = typ.wrapped(field_apply[T,R](struct.s, name))


  @internal private[argon] def equals[T:StructType](a: T, b: T): Bool = {
    val tp = implicitly[StructType[T]]
    def eql[F:Type](x: F, y: F): Bool = x === y

    tp.fields.map {case (name, fieldTyp) =>
      implicit val mA: Type[_] = fieldTyp
      val x = field(a, name, mA)
      val y = field(b, name, mA)
      eql(x,y)(mtyp(fieldTyp))
    }.reduce(_&&_)
  }
  @internal private[argon] def unequals[T:StructType](a: T, b: T): Bool = {
    val tp = implicitly[StructType[T]]
    def neq[F:Type](x: F, y: F): Bool = x =!= y

    tp.fields.map {case (name, fieldTyp) =>
      implicit val mA: Type[_] = fieldTyp
      val x = field(a, name, mA)
      val y = field(b, name, mA)
      neq(x,y)(mtyp(fieldTyp))
    }.reduce(_||_)
  }

  private[argon] def unwrapStruct[S:StructType,T:Type](struct: Exp[S], index: String): Option[Exp[T]] = struct match {
    case Op(Struct(elems)) => elems.get(index) match {
      case Some(x) if x.tp <:< typ[T] => Some(x.asInstanceOf[Exp[T]]) // TODO: Should this be Staged asInstanceOf?
      case None =>
        throw new NoFieldException(struct, index) // TODO: Should this be a user error?
    }
    case _ => None
  }


  /** Constructors **/
  def struct_new[S:StructType](elems: Seq[(String, Exp[_])])(implicit ctx: SrcCtx): Exp[S] = {
    stage(SimpleStruct(elems))(ctx)
  }

  // TODO: Should struct unwrapping be disabled for mutable structs?
  @internal def field_apply[S:StructType,T:Type](struct: Exp[S], index: String): Exp[T] = struct match {
    case Op(s:StructAlloc[_]) if Config.unwrapStructs => unwrapStruct[S,T](struct, index) match {
      case Some(x) => x
      case None => stage(FieldApply[S,T](struct, index))(ctx)
    }
    case _ => stage(FieldApply[S,T](struct, index))(ctx)
  }
  @internal def field_update[S:StructType,T:Type](struct: Exp[S], index: String, value: Exp[T]): Exp[Void] = {
    stageWrite(struct)(FieldUpdate(struct, index, value))(ctx)
  }
}



/** IR Nodes **/
abstract class StructAlloc[T:StructType] extends Op[T] {
  def elems: Seq[(String, Exp[_])]

  override def inputs   = dyns(elems.map(_._2))
  override def reads    = Nil
  override def freqs    = normal(elems.map(_._2))

  override def aliases  = Nil
  override def contains = dyns(elems.map(_._2))
}

case class SimpleStruct[S:StructType](elems: Seq[(String,Exp[_])]) extends StructAlloc[S] {
  def mirror(f:Tx) = Struct.struct_new[S](elems.map{case (idx,sym) => idx -> f(sym) })
}

case class FieldApply[S:StructType,T:Type](coll: Exp[S], field: String) extends Op2[S,T] with AtomicRead[S] {
  def mirror(f:Tx) = Struct.field_apply[S,T](f(coll), field)

  override def extracts = dyns(coll)
}
case class FieldUpdate[S:StructType,T:Type](struct: Exp[S], field: String, value: Exp[T]) extends Op3[S,T,Void] {
  def mirror(f:Tx) = Struct.field_update(f(struct), field, f(value))

  override def contains = dyns(value)  // TODO: Is this necessary?
}