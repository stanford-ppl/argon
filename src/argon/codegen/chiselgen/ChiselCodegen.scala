package argon.codegen.chiselgen

import argon.codegen.Codegen
import argon.Config


trait ChiselCodegen extends Codegen {
  import IR._
  override val name = "Chisel Codegen"
  override val lang: String = "chisel"
  override val ext: String = "scala"
  var emitEn: Boolean = false // Hack for masking Cpp from FPGA gen

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"// results in ${b.result}")
  }

  final protected def emitController(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"// results in ${b.result}")
  }

  override def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case lhs: Sym[_] => s"x${lhs.id}"
  }

  final protected def emitGlobal(x: String): Unit = { 
    withStream(getStream("GlobalWires")) {
      stream.println(x) 
    }
  }

  override def emit(x: String): Unit = { if (emitEn) stream.println(tabbed + x) }
  override def open(x: String): Unit = { 
    if (emitEn) {
      stream.println(tabbed + x); if (streamTab contains streamName) streamTab(streamName) += 1 
    }
  }
  override def close(x: String): Unit = { 
    if (emitEn) {
      if (streamTab contains streamName) streamTab(streamName) -= 1; stream.println(tabbed + x)  
    }
  }
  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = {
    if (emitEn) {throw new GenerationFailedException(rhs)} else {Console.println(s"[WARN] no backend for $lhs = $rhs in $lang")}
  }


  final protected def withSubStream[A](name: String)(body: => A): A = { // Places body inside its own trait file and includes it at the end
    if (Config.multifile) {
      withStream(newStream(name)) {
          emit("""package app
import templates._
import chisel3._""")
          open(s"""trait ${name} extends BaseModule with TopModuleTrait /*and possibly other subkernels up to this point*/ {""")
          open(s"""def create_${name}() {""")
          try { body } 
          finally { 
            close("}")
            close("}")
          }
      }
    } else {
      open("{ // Multifile disabled, emitting here")
      try { body } 
      finally { close("}") }
    }
  }





}
