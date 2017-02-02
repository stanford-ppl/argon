package argon.codegen.chiselgen

import argon.codegen.FileGen

trait ChiselFileGen extends FileGen {
  import IR._


  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def process[S:Staged](b: Block[S]): Block[S] = {

    // // Forcefully create the following streams
    // val baseStream = getStream("GlobalWires")
    // val ioModule = getStream("IOModule")
    // val topModule = getStream("TopLevelDesign")
    // val bufferControl = getStream("BufferControlCxns")
    // val topTrait = getStream("TopTrait")

    withStream(getStream("TopTrait")) {
      preprocess(b)
      emitMain(b)
      postprocess(b)
    }
  }


  override protected def emitFileHeader() {

    withStream(getStream("IOModule")) {
      emit(s"""package interfaces
import chisel3._
import templates._
import types._
class ArgInBundle() extends Bundle{""")
    }

    withStream(getStream("BufferControlCxns")) {
      emit(s"""package app
import templates._
import chisel3._""")
      open(s"""trait BufferControlSignals extends BaseModule with TopModuleTrait /*and possibly other subkernels up to this point*/ {""")
      open(s"""def create_BufferControlSignals() {""")
    }

    withStream(getStream("TopTrait")) {
      emit(s"""package app
import templates._
import interfaces._
import chisel3._""")
      open(s"trait TopModuleTrait extends BaseModule /*and possibly subkernels*/ {")
      emit(s"// May want to have a main method defined here too")
    }

    withStream(getStream("GlobalWires")) {
      emit(s"""package app
import templates._
import interfaces._
import chisel3._
abstract class BaseModule() extends Module{
  val io = IO(new Bundle{
    val top_en = Input(Bool())
    val top_done = Output(Bool())
    val ArgIn = new ArgInBundle()
    val ArgOut = new ArgOutBundle()
    val MemStreams = new MemStreamsBundle()
  })
""")
    }

    super.emitFileHeader()
  }

  override protected def emitFileFooter() {
    // emitBufferControlSignals()
    withStream(getStream("GlobalWires")) {
      // // Get each all unique reg strings
      // emitted_argins.toList.map{a=>a._2}.distinct.foreach{ a => 
      //   emit(s"""val ${a} = io.ArgIn.ports(${argInsByName.indexOf(a)})""")
      // }

      // emitted_argins.toList.foreach {
      //   case (sym, regStr) =>
      //     emit(s"""val ${quote(sym)} = $regStr""")
      // }
      emit("}")
    }

    withStream(getStream("TopTrait")) {
      emit(s"// Would close main method here")
      close(s"}")
    }

    withStream(getStream("BufferControlCxns")) {
      close("}")
      close("}")
    }

    // Get traits that need to be mixed in
    val traits = streamMapReverse.keySet.toSet - "TopLevelDesign" - "IOModule" - "GlobalWires" - "TopTrait"
    withStream(getStream("TopLevelDesign")) {
      emit(s"""package app
import templates._
import interfaces._
import chisel3._
class TopModule() extends GlobalWires with ${(traits+"TopTrait").mkString("\n with ")} {
  ${traits.map{ a => s"  create_${a}()"}.mkString("\n") }
}
  // TopModule class mixes in all the other traits and is instantiated by tester""")
    }

    withStream(getStream("IOModule")) {
      emit("}")
    }
    super.emitFileFooter()
  }

}
