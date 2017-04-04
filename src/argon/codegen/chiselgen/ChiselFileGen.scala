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
    // val AccelTop = getStream("AccelTop")
    // val bufferControl = getStream("BufferControlCxns")
    // val RootController = getStream("RootController")

    withStream(getStream("RootController")) {
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
      emit(s"""package accel
import chisel3._
import templates._
import templates.ops._
import chisel3.util._
import fringe._
import types._""")
      open("trait IOModule extends Module {")
      emit("""val target = "" // TODO: Get this info from command line args (aws, de1, etc)""")
      emit("val io_w = 64 // TODO: How to generate these properly?")
      emit("val io_v = 16 // TODO: How to generate these properly?")
    }

    withStream(getStream("BufferControlCxns")) {
      emit(s"""package accel
import templates._
import templates.ops._
import fringe._
import chisel3._""")
      open(s"""trait BufferControlCxns extends RootController {""")
    }

    withStream(getStream("RootController")) {
      emit(s"""package accel
import templates._
import templates.ops._
import fringe._
import types._
import chisel3._""")
      open(s"trait RootController extends GlobalWires {")
      emit(src"// Root controller for app: ${Config.name}")

    }

    withStream(getStream("GlobalWires")) {
      emit(s"""package accel
import templates._
import templates.ops._
import chisel3._
import types._
trait GlobalWires extends IOModule{""")
    }

    withStream(getStream("Instantiator")) {
      emit("// See LICENSE for license details.")
      emit("")
      emit("package top")
      emit("")
      emit("import fringe._")
      emit("import accel._")
      emit("import chisel3.core.Module")
      emit("import chisel3._")
      emit("import chisel3.util._")
      emit("import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}")
      emit("")
      emit("import scala.collection.mutable.ListBuffer")

      emit("/**")
      emit(" * Top test harness")
      emit(" */")
      open("class TopUnitTester(c: Top)(implicit args: Array[String]) extends ArgsTester(c) {")
      close("}")
      emit("")
      open("object Instantiator extends CommonMain {")
        emit("type DUTType = Top")
        emit("")
        open("def dut = () => {")

    }


    super.emitFileHeader()
  }

  override protected def emitFileFooter() {
    // emitBufferControlCxns()

    withStream(getStream("Instantiator")) {
          emit("val w = 32")
          emit("val numArgIns = numArgIns_mem  + numArgIns_reg")
          emit("val numArgOuts = numArgOuts_reg")
          emit("new Top(w, numArgIns, numArgOuts, loadStreamInfo, storeStreamInfo, streamInsInfo, streamOutsInfo, target)")
        close("}")
        emit("def tester = { c: DUTType => new TopUnitTester(c) }")
      close("}")

    }
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

    withStream(getStream("IOModule")) {
      emit("// Combine values")
      emit("val io_numArgIns = io_numArgIns_reg + io_numArgIns_mem")
      emit("val io_numArgOuts = io_numArgOuts_reg")
      open("val io = IO(new Bundle {")
        emit("// Control")
        emit("val enable = Input(Bool())")
        emit("val done = Output(Bool())")
        emit("")
        emit("// Tile Load")
        emit("val memStreams = Flipped(new AppStreams(io_loadStreamInfo, io_storeStreamInfo))")
        emit("")
        emit("// Scalars")
        emit("val argIns = Input(Vec(io_numArgIns, UInt(64.W)))")
        emit("val argOuts = Vec(io_numArgOuts, Decoupled((UInt(64.W))))")
        emit("")
        emit("// Streams")
        emit("val genericStreams = Flipped(new GenericStreams(io_streamInsInfo, io_streamOutsInfo))")
        emit("")
      close("})")
      close("}")
    }

    withStream(getStream("RootController")) {
      close(s"}")
    }

    withStream(getStream("BufferControlCxns")) {
      close("}")
    }

    if (Config.multifile >= 3 ) {
      val traits = streamMapReverse.keySet.toSet.map{
        f:String => f.split('.').dropRight(1).mkString(".")  /*strip extension */ 
      }.toSet - "AccelTop" - "GlobalWires" - "Instantiator"

      withStream(getStream("AccelTop")) {
        emit(s"""package accel
import templates._
import fringe._
import chisel3._
import chisel3.util._
class AccelTop(
  val top_w: Int,
  val numArgIns: Int,
  val numArgOuts: Int,
  val loadStreamInfo: List[StreamParInfo],
  val storeStreamInfo: List[StreamParInfo],
  val streamInsInfo: List[StreamParInfo],
  val streamOutsInfo: List[StreamParInfo]
) extends GlobalWires with ${(traits++Set("RootController")).mkString("\n with ")} {

  // TODO: Figure out better way to pass constructor args to IOModule.  Currently just recreate args inside IOModule redundantly

}""")
      }
    } else {
    // Get traits that need to be mixed in
      val traits = streamMapReverse.keySet.toSet.map{
        f:String => f.split('.').dropRight(1).mkString(".")  /*strip extension */ 
      }.toSet - "AccelTop" - "GlobalWires" - "RootController" - "Instantiator"
      withStream(getStream("AccelTop")) {
        emit(s"""package accel
import templates._
import fringe._
import chisel3._
import chisel3.util._

class AccelTop(
  val top_w: Int,
  val numArgIns: Int,
  val numArgOuts: Int,
  val loadStreamInfo: List[StreamParInfo],
  val storeStreamInfo: List[StreamParInfo],
  val numStreamIns: List[StreamParInfo],
  val numStreamOuts: List[StreamParInfo]
) extends GlobalWires with ${(traits++Set("RootController")).mkString("\n with ")} {

}
  // AccelTop class mixes in all the other traits and is instantiated by tester""")
      }
        
    }


    super.emitFileFooter()
  }

}
