package argon.codegen.chiselgen

import argon.codegen.FileGen
import argon.Config

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
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen-NOTE ] Begin!")}
      preprocess(b)
      toggleEn() // Turn off
      emitMain(b)
      toggleEn() // Turn on
      postprocess(b)
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen-NOTE ] Complete!")}
      b
    }
  }


  override protected def emitFileHeader() {

    withStream(getStream("IOModule")) {
      emit(s"""package interfaces
import chisel3._
import templates._
import types._""")
    }

    withStream(getStream("BufferControlCxns")) {
      emit(s"""package app
import templates._
import chisel3._""")
      open(s"""trait BufferControlCxns extends GlobalWires with TopTrait /*and possibly other subkernels up to this point*/ {""")
      open(s"""def create_BufferControlCxns() {""")
    }

    withStream(getStream("TopTrait")) {
      emit(s"""package app
import templates._
import interfaces._
import chisel3._""")
      open(s"trait TopTrait extends GlobalWires /*and possibly subkernels*/ {")
      emit(s"// May want to have a main method defined here too")
    }

    withStream(getStream("GlobalWires")) {
      emit(s"""package app
import templates._
import interfaces._
import chisel3._
abstract class GlobalWires() extends Module{
  val io = IO(new Bundle{
    val top_en = Input(Bool())
    val top_done = Output(Bool())
    val ArgIn = new ArgInBundle()
    val ArgOut = new ArgOutBundle()
    val MemStreams = new MemStreamsBundle()
    val StreamIns = new StreamInsBundle()
    val StreamOuts = new StreamOutsBundle()
  })
""")
    }

//    withStream(getStream("GeneratedPoker")) {
//      emit(s"""package app
//
//import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
//import org.scalatest.Assertions._
//import java.io._""")
//      open(s"""class GeneratedPoker(c: TopModule) extends PeekPokeTester(c) {""")
//      emit(s"""var offchipMem = List[BigInt]()""")
//      open(s"def handleLoadStore() {")
//    }

    super.emitFileHeader()
  }

  override protected def emitFileFooter() {
    // emitBufferControlCxns()
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
    val traits = streamMapReverse.keySet.toSet.map{
      f:String => f.split('.').dropRight(1).mkString(".")  /*strip extension */ 
    }.toSet - "TopLevelDesign" - "IOModule" - "GlobalWires" - "TopTrait" // - "GeneratedPoker"
    withStream(getStream("TopLevelDesign")) {
      emit(s"""package app
import templates._
import interfaces._
import chisel3._
class AccelTop(val w: Int, val numArgIns: Int, val numArgOuts: Int, val numMemoryStreams: Int) extends GlobalWires with ${(traits++Set("TopTrait")).mkString("\n with ")} {
  ${traits.map{ a => s"  create_${a}()"}.mkString("\n") }
}
  // TopModule class mixes in all the other traits and is instantiated by tester""")
    }

//    withStream(getStream("GeneratedPoker")) {
//      close("}")
//      close("}")
//    }

    super.emitFileFooter()
  }

}
