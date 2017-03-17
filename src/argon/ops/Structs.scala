package argon.ops

// import org.virtualized.{RefinedManifest,RecordOps,Record}
import argon.Config
import argon.core.Staging

trait StructApi extends StructExp with VoidApi

trait StructExp extends Staging with VoidExp {

  abstract class StructApi[T <: StageAny[T]  :StructType] { self =>
    def field[R <: StageAny[R] : FStaged](name: String)(implicit ctx: SrcCtx): R = wrap(field_apply[T,R](self.asInstanceOf[T].s, name))
  }

  abstract class StructType[T <: StageAny[T]] extends FStaged[T] {
    override def isPrimitive = false
    def fields: Seq[(String, FStaged[_])]
  }

  // def record_new[T: RefinedManifest](fields: (String, _)*): T
  // def record_select[T: Manifest](record: Record, field: String): T
  def struct[T <: StageAny[T] :StructType](fields: (String, Exp[_])*)(implicit ctx: SrcCtx): T = wrap(struct_new[T](fields))
  def field[T <: StageAny[T] :StructType,R <: StageAny[R] : FStaged](struct: T, name: String)(implicit ctx: SrcCtx): R = wrap(field_apply[T,R](struct.s, name))

  /** IR Nodes **/
  abstract class StructAlloc[T <: StageAny[T] : StructType] extends Op[T] {
    def elems: Seq[(String, Exp[_])]

    override def inputs   = syms(elems.map(_._2))
    override def reads    = Nil
    override def freqs    = normal(elems.map(_._2))

    override def aliases  = Nil
    override def contains = syms(elems.map(_._2))
  }

  case class SimpleStruct[S <: StageAny[S] : StructType](elems: Seq[(String,Exp[_])]) extends StructAlloc[S] {
    def mirror(f:Tx) = struct_new[S](elems.map{case (idx,sym) => idx -> f(sym) })
  }

  case class FieldApply[S <: StageAny[S] :StructType,T <: StageAny[T] : FStaged](struct: Exp[S], field: String) extends Op2[S,T] {
    def mirror(f:Tx) = field_apply[S,T](f(struct), field)

    override def extracts = syms(struct)
  }
  case class FieldUpdate[S <: StageAny[S] : StructType,T <: StageAny[T] : FStaged](struct: Exp[S], field: String, value: Exp[T]) extends Op3[S,T,Void] {
    def mirror(f:Tx) = field_update(f(struct), field, f(value))

    override def contains = syms(value)  // TODO: Is this necessary?
  }


  /** Constructors **/
  def struct_new[S <: StageAny[S] : StructType](elems: Seq[(String, Exp[_])])(implicit ctx: SrcCtx): Exp[S] = {
    stage(SimpleStruct(elems))(ctx)
  }

  // TODO: Should struct unwrapping be disabled for mutable structs?
  def field_apply[S <: StageAny[S] :StructType,T <: StageAny[T] : FStaged](struct: Exp[S], index: String)(implicit ctx: SrcCtx): Exp[T] = struct match {
    case Op(s:StructAlloc[_]) if Config.unwrapStructs => unwrapStruct[S,T](struct, index) match {
      case Some(x) => x
      case None => stage(FieldApply[S,T](struct, index))(ctx)
    }
    case _ => stage(FieldApply[S,T](struct, index))(ctx)
  }
  def field_update[S <: StageAny[S] : StructType,T <: StageAny[T] : FStaged](struct: Exp[S], index: String, value: Exp[T])(implicit ctx: SrcCtx): Exp[Void] = {
    stageWrite(struct)(FieldUpdate(struct, index, value))(ctx)
  }

  /** Helper functions **/
  object Struct {
    def unapply(x: Op[_]): Option[Map[String,Exp[_]]] = x match {
      case s: StructAlloc[_] => Some(s.elems.toMap)
      case _ => None
    }
  }

  def unwrapStruct[S <: StageAny[S]:StructType,T <: StageAny[T] : FStaged](struct: Exp[S], index: String): Option[Exp[T]] = struct match {
    case Op(Struct(elems)) => elems.get(index) match {
      case Some(x) if x.tp <:< ftyp[T] => Some(x.asInstanceOf[Exp[T]]) // TODO: Should this be FStaged asInstanceOf?
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
