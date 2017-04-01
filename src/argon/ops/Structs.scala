package argon.ops

import argon.Config
import argon.core.Staging
import forge._

trait StructApi extends StructExp with VoidApi{
  self: TextApi =>
}

trait StructExp extends Staging with VoidExp with TextExp {

  /**
    * Parent class for all staged Structs
    */
  abstract class MetaStruct[T:StructType] extends MetaAny[T]{ self =>
    protected def tp = implicitly[StructType[T]]
    protected def field[R:Meta](name: String)(implicit ctx: SrcCtx): R = wrap(field_apply[T,R](self.s, name))
    protected def fieldToText[T](name: String, tp: Meta[T])(implicit ctx: SrcCtx) = tp.ev(field(name)(mtyp(tp), ctx)).toText

    @api def =!=(that: T): Bool = struct_unequals(this.asInstanceOf[T],that)
    @api def ===(that: T): Bool = struct_equals(this.asInstanceOf[T],that)
    @api def toText = {
      val fields = tp.fields.map{case (name,fieldTyp) => fieldToText(name,fieldTyp) }
      lift[String,Text](tp.prefix + "(") + fields.reduceLeft{(a,b) => a + "," + b } + ")"
    }
  }

  def struct_equals[T:StructType](a: T, b: T)(implicit ctx: SrcCtx): Bool = {
    val tp = implicitly[StructType[T]]
    def eql[F:Meta](x: F, y: F): Bool = x === y

    tp.fields.map {case (name, fieldTyp) =>
      implicit val mA: Meta[_] = fieldTyp
      val x = field(a, name)(tp, fieldTyp, ctx)
      val y = field(b, name)(tp, fieldTyp, ctx)
      eql(x,y)(mmeta(fieldTyp))
    }.reduce(_&&_)
  }
  def struct_unequals[T:StructType](a: T, b: T)(implicit ctx: SrcCtx): Bool = {
    val tp = implicitly[StructType[T]]
    def neq[F:Meta](x: F, y: F): Bool = x =!= y

    tp.fields.map {case (name, fieldTyp) =>
      implicit val mA: Meta[_] = fieldTyp
      val x = field(a, name)(tp, fieldTyp, ctx)
      val y = field(b, name)(tp, fieldTyp, ctx)
      neq(x,y)(mmeta(fieldTyp))
    }.reduce(_||_)
  }



  abstract class StructType[T](implicit ev: T <:< MetaStruct[T]) extends Meta[T] {
    override def isPrimitive = false
    def fields: Seq[(String, Meta[_])]
    def prefix: String = this.stagedClass.getSimpleName
  }

  // def record_new[T: RefinedManifest](fields: (String, _)*): T
  // def record_select[T: Manifest](record: Record, field: String): T
  def struct[T:StructType](fields: (String, Exp[_])*)(implicit ctx: SrcCtx): T = wrap(struct_new[T](fields))
  def field[T:StructType,R:Meta](struct: T, name: String)(implicit ctx: SrcCtx): R = wrap(field_apply[T,R](struct.s, name))


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
    def mirror(f:Tx) = struct_new[S](elems.map{case (idx,sym) => idx -> f(sym) })
  }

  case class FieldApply[S:StructType,T:Type](struct: Exp[S], field: String) extends Op2[S,T] {
    def mirror(f:Tx) = field_apply[S,T](f(struct), field)

    override def extracts = dyns(struct)
  }
  case class FieldUpdate[S:StructType,T:Type](struct: Exp[S], field: String, value: Exp[T]) extends Op3[S,T,Void] {
    def mirror(f:Tx) = field_update(f(struct), field, f(value))

    override def contains = dyns(value)  // TODO: Is this necessary?
  }


  /** Constructors **/
  def struct_new[S:StructType](elems: Seq[(String, Exp[_])])(implicit ctx: SrcCtx): Exp[S] = {
    stage(SimpleStruct(elems))(ctx)
  }

  // TODO: Should struct unwrapping be disabled for mutable structs?
  def field_apply[S:StructType,T:Type](struct: Exp[S], index: String)(implicit ctx: SrcCtx): Exp[T] = struct match {
    case Op(s:StructAlloc[_]) if Config.unwrapStructs => unwrapStruct[S,T](struct, index) match {
      case Some(x) => x
      case None => stage(FieldApply[S,T](struct, index))(ctx)
    }
    case _ => stage(FieldApply[S,T](struct, index))(ctx)
  }
  def field_update[S:StructType,T:Type](struct: Exp[S], index: String, value: Exp[T])(implicit ctx: SrcCtx): Exp[Void] = {
    stageWrite(struct)(FieldUpdate(struct, index, value))(ctx)
  }

  /** Helper functions **/
  object Struct {
    def unapply(x: Op[_]): Option[Map[String,Exp[_]]] = x match {
      case s: StructAlloc[_] => Some(s.elems.toMap)
      case _ => None
    }
  }

  def unwrapStruct[S:StructType,T:Type](struct: Exp[S], index: String): Option[Exp[T]] = struct match {
    case Op(Struct(elems)) => elems.get(index) match {
      case Some(x) if x.tp <:< typ[T] => Some(x.asInstanceOf[Exp[T]]) // TODO: Should this be Staged asInstanceOf?
      case None =>
        throw new NoFieldException(struct, index) // TODO: Should this be a user error?
    }
    case _ => None
  }

  /** Internals **/
  override def recurseAtomicLookup(s: Exp[_]): Exp[_] = s match {
    case Def(FieldApply(struct, index)) => recurseAtomicLookup(struct)
    case _ => super.recurseAtomicLookup(s)
  }

}
