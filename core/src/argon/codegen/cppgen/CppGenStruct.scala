package argon.codegen.cppgen

import argon.core._
import argon.codegen.StructCodegen
import argon.nodes._

import scala.language.postfixOps
import sys.process._

trait CppGenStruct extends CppCodegen with StructCodegen {

  protected def structName(tp: StructType[_], idx: Int): String = s"Struct$idx"

  protected def bw(tp: Type[_]): Int = tp match {
    case IntType() => 32
    case LongType() => 64
    case FixPtType(s,d,f) => d+f
    case BooleanType() => 1
    case _ => throw new Exception(s"No bitwidth for $tp")
  }

  protected def emitStructDeclaration(name: String, tp: StructType[_]): Unit = {
    // Create struct
    open(src"class $name {")
    open("public:")
    val argarray = tp.fields.map{case (field, t) => src"$t $field"}
    argarray.foreach{ line => emit(src"$line;") }
    open(src"""$name(${argarray.map{arg => src"${arg}_in"}.mkString(",")}) {""")
    tp.fields.foreach{case (field, t) => emit(src"this->$field = ${field}_in;")}
    close("}")
    val width = tp.fields.map{case (a,b) => bw(b)}.sum
    val rawtp = if (width > 64) "int128_t"
        else if (width > 32) "int64_t"
        else if (width > 16) "int32_t"
        else if (width > 8) "int16_t"
        else if (width > 4) "int8_t"
        else if (width > 2) "int2_t"
        else "boolean"
    open(src"$name($rawtp x) {")
      var position = 0
      tp.fields.foreach{case (field, t) => 
        emit(src"this->${field} = ($t) (x >> $position); ")
        position = position + bw(t)
      }
      // emit(src"return $name(${argarray.map{arg => src"${arg}_fromRaw"}.mkString(",")});")
    close("}")

    emit(src"""$name() { } // For creating empty array """)
    tp.fields.foreach{case (field, t) => emit(src"void set$field($t num) {this->$field = num;}")}
    tp.fields.foreach{case (field, t) => emit(src"$t get$field() {return this->$field;}")}
    open(src"$rawtp toRaw() {")
      emit(src"$rawtp result = 0;")
      position = 0
      tp.fields.foreach{case (field, t) => 
        emit(src"result = result | (($rawtp) (this->$field) << $position); ")
        position = position + bw(t)
      }
      emit(src"return result;")
    close("}")
    close(" ")
    close("};")
  }

  protected def emitArrayStructDeclaration(name: String, tp: StructType[_]): Unit = {
    open(src"class cppDeliteArray$name {")
    open("public:")
    emit(src"""${name} *data;""")
    emit(src"""int length;""")
    emit(src"""""")
    emit(src"""cppDeliteArray${name}(int _length): data((${name}  *)(new ${name} [_length])), length(_length) { }""")
    emit(src"""""")
    open(src"""cppDeliteArray${name}(${name}  *_data, int _length) {""")
    emit(src"""data = _data;""")
    emit(src"""length = _length;""")
    close(src"""}""")
    emit(src"""""")
    open(src"""${name}  apply(int idx) {""")
    emit(src"""return data[idx];""")
    close(src"""}""")
    emit(src"""""")
    open(src"""void update(int idx, ${name}  val) {""")
    emit(src"""data[idx] = val;""")
    close(src"""}""")
    emit(src"""""")
    open(src"""void print(void) {""")
    emit(src"""printf("length is %d\n", length);""")
    close(src"""}""")
    emit(src"""""")
    open(src"""bool equals(cppDeliteArray${name} *to) {""")
    emit(src"""return this == this;""")
    close(src"""}""")
    emit(src"""""")
    open(src"""uint32_t hashcode(void) {""")
    emit(src"""return (uintptr_t)this;""")
    close(src"""}""")
    emit(src"""""")
    open(src"""#ifdef DELITE_GC""")
    emit(src"""void deepCopy(void) {""")
    close(src"""}""")
    emit(src"""#endif""")
    emit(src"""""")
    open(src"""struct cppDeliteArray${name}D {""")
    open(src"""void operator()(cppDeliteArray${name} *p) {""")
    emit(src"""//printf("cppDeliteArray${name}: deleting %p\n",p);""")
    emit(src"""delete[] p->data;""")
    close(src"""}""")
    close(src"""};""")
    close("")
    close("};")
  }


  protected def emitDataStructures(): Unit = if (encounteredStructs.nonEmpty) {
    withStream(newStream("Structs", "h")) {
      emit("// Codegenerated types")
      emit(s"#include <stdint.h>")
      emit(s"#include <vector>")
      emit(s"#include <iostream>")
      emit("#ifndef ZYNQ")
      emit("typedef __int128 int128_t;")
      emit("#endif")

      withStream(getStream("cpptypes","h")) {emit(src"#include <Structs.h>")}
      for ((tp, name) <- encounteredStructs) {
        emitStructDeclaration(name, tp)
        emit("")
      }
    }
    // // TODO: Matt!
    // dependencies ::= FileDep("cppgen", "Structs.h")

    withStream(newStream("cppDeliteArrayStructs","h")) {
      emit("// Codegenerated types")
      withStream(getStream("cpptypes","h")) {emit(src"#include <cppDeliteArrayStructs.h>")}
      for ((tp, name) <- encounteredStructs) {
        emitArrayStructDeclaration(name, tp)
        emit("")
      }
    }
    // TODO: Matt!
    // dependencies ::= FileDep("cppgen", "cppDeliteArrayStructs.h")

  }

  override protected def remap(tp: Type[_]): String = tp match {
    // case TupleType[]
    // case IntType() => "Int"
    // case LongType() => "Long"
    case _ => super.remap(tp)
  }


  override protected def quoteConst(c: Const[_]): String = (c.tp, c) match {
    case (st: StructType[_], e@Const(elems)) =>
      val seq = elems.asInstanceOf[Seq[(_, Exp[_])]]
      src"*(new ${st}( " + seq.map(x => quote(x._2)).mkString(", ") + " ));"

    case _ => super.quoteConst(c)
  }
  

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]) = rhs match {
    case e: StructAlloc[_] =>
      emit(src"${lhs.tp} $lhs = *(new ${e.mR}( " + e.elems.map(x => quote(x._2)).mkString(", ") + " ));")

    case FieldUpdate(struct, field, value) => emit(src"${lhs.tp} $lhs = $struct.set$field($value);")
    case FieldApply(struct, field)         => emit(src"${lhs.tp} $lhs = $struct.get$field();")

    case _ => super.emitNode(lhs, rhs)
  }


}
