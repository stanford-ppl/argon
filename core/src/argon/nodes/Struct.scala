package argon.nodes

import argon.core._
import argon.compiler._

abstract class StructType[T](override implicit val ev: T <:< Struct[T]) extends Type[T] {
  override def isPrimitive = false
  def fields: Seq[(String, Type[_])]
  def prefix: String = this.stagedClass.getSimpleName
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
case class FieldUpdate[S:StructType,T:Type](struct: Exp[S], field: String, value: Exp[T]) extends Op3[S,T,MUnit] {
  def mirror(f:Tx) = Struct.field_update(f(struct), field, f(value))

  override def contains = dyns(value)  // TODO: Is this necessary?
}