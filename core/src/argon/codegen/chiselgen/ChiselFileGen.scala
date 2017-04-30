package argon.codegen.chiselgen

import argon.codegen.FileGen
import argon.Config

trait ChiselFileGen extends FileGen {
  import IR._


  override protected def emitMain[S:Type](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def process[S:Type](b: Block[S]): Block[S] = {

    // // Forcefully create the following streams
    // val baseStream = getStream("GlobalWires")
    // val ioModule = getStream("IOModule")
    // val AccelTop = getStream("AccelTop")
    // val bufferControl = getStream("BufferControlCxns")
    // val RootController = getStream("RootController")

    withStream(getStream("RootController")) {
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Begin!")}
      preprocess(b)
      toggleEn() // Turn off
      emitMain(b)
      toggleEn() // Turn on
      postprocess(b)
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Complete!")}
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

    val gw_extensions = (0 until numGlobalFiles).map{j => "GlobalWires" + j}.mkString(" with ")
    val gm_extensions = (0 until numGlobalFiles).map{j => "GlobalModules" + j}.mkString(" with ")

    withStream(getStream("RootController")) {
      emit(s"""package accel
import templates._
import templates.ops._
import fringe._
import types._
import chisel3._""")
      open(s"trait RootController extends ${gm_extensions} with GlobalRetiming {")
      emit(src"// Root controller for app: ${Config.name}")

    }

    for (i <- 0 until numGlobalFiles) {
      withStream(getStream("GlobalWires"+i)) {
        emit(s"""package accel
import templates._
import templates.ops._
import chisel3._
import types._""")
        open(s"""trait GlobalWires$i extends IOModule{""")
      }

    }

    for (i <- 0 until numGlobalFiles) {
      withStream(getStream("GlobalModules" + i)) {
        emit(s"""package accel
import templates._
import templates.ops._
import chisel3._
import types._ """)
        open(s"""trait GlobalModules$i extends ${gw_extensions} {""")
      }
    }

    withStream(getStream("GlobalRetiming")) {
      emit(s"""package accel
import templates._
import templates.ops._
import chisel3._
import types._""")
      open(s"""trait GlobalRetiming extends ${gw_extensions} {""")
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
          emit("val numArgIns = numArgIns_mem  + numArgIns_reg + numArgIOs_reg")
          emit("val numArgOuts = numArgOuts_reg + numArgIOs_reg")
          emit("val numArgIOs = numArgIOs_reg")
          emit("new Top(w, numArgIns, numArgOuts, numArgIOs, loadStreamInfo, storeStreamInfo, streamInsInfo, streamOutsInfo, target)")
        close("}")
        emit("def tester = { c: DUTType => new TopUnitTester(c) }")
      close("}")

    }
    for (i <- 0 until numGlobalFiles) {
      withStream(getStream("GlobalWires"+ i)) {
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
    }

    withStream(getStream("IOModule")) {
      emit("// Combine values")
      emit("val io_numArgIns = io_numArgIns_reg + io_numArgIns_mem + io_numArgIOs_reg")
      emit("val io_numArgOuts = io_numArgOuts_reg + io_numArgIOs_reg")
      emit("val io_numArgIOs = io_numArgIOs_reg")
      open("val io = IO(new Bundle {")
        emit("// Control IO")
        emit("val enable = Input(Bool())")
        emit("val done = Output(Bool())")
        emit("")
        emit("// DRAM IO")
        emit("val memStreams = Flipped(new AppStreams(io_loadStreamInfo, io_storeStreamInfo))")
        emit("")
        emit("// Scalar IO")
        emit("val argIns = Input(Vec(io_numArgIns, UInt(64.W)))")
        emit("val argOuts = Vec(io_numArgOuts, Decoupled((UInt(64.W))))")
        emit("")
        emit("// Stream IO")
        emit("val genericStreams = new GenericStreams(io_streamInsInfo, io_streamOutsInfo)")
        emit("// Video Stream Inputs ")
        emit("val stream_in_data            = Input(UInt(24.W))")
        emit("val stream_in_startofpacket   = Input(Bool())")
        emit("val stream_in_endofpacket     = Input(Bool())")
        emit("val stream_in_empty           = Input(UInt(2.W))")
        emit("val stream_in_valid           = Input(Bool()) ")
        emit("val stream_out_ready          = Input(Bool())")
        emit(" ")
        emit("// Video Stream Outputs")
        emit("val stream_in_ready           = Output(Bool())")
        emit("val stream_out_data           = Output(UInt(16.W))")
        emit("val stream_out_startofpacket  = Output(Bool())")
        emit("val stream_out_endofpacket    = Output(Bool())")
        emit("val stream_out_empty          = Output(UInt(1.W))")
        emit("val stream_out_valid          = Output(Bool())")
        emit("")
        emit("// LED Stream Outputs ")
        emit("val led_stream_out_data       = Output(UInt(32.W))")
        emit("")
        emit("// Slider Switches Stream Inputs ")
        emit("val switch_stream_in_data     = Input(UInt(32.W))")
      close("})")
      emit("var outMuxMap: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map[Int,Int]()")
      emit("(0 until io_numArgOuts).foreach{ i => outMuxMap += (i -> 0) }")
      open("def getArgOutLane(id: Int): Int = {")
        emit("val lane = outMuxMap(id)")
        emit("outMuxMap += (id -> {lane + 1})")
        emit("lane")
      close("}")
      // emit("var loadMuxMap: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map[Int,Int]()")
      // emit("(0 until io_streamInsInfo.length).foreach{ i => loadMuxMap += (i -> 0) }")
      // open("def getLoadLane(id: Int): Int = {")
      //   emit("val lane = loadMuxMap(id)")
      //   emit("loadMuxMap += (id -> {lane + 1})")
      //   emit("lane")
      // close("}")
      // emit("var storeMuxMap: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map[Int,Int]()")
      // emit("(0 until io_streamOutsInfo.length).foreach{ i => storeMuxMap += (i -> 0) }")
      // open("def getStoreLane(id: Int): Int = {")
      //   emit("val lane = storeMuxMap(id)")
      //   emit("storeMuxMap += (id -> {lane + 1})")
      //   emit("lane")
      // close("}")
      close("}")
    }

    streamExtensions("RootController").foreach{i => 
      val fname = if (i == 0) "RootController" else src"RootController_${i}"
      withStream(getStream(fname)) { stream.println("}")}
    }

    withStream(getStream("BufferControlCxns")) {
      close("}")
    }

    for (i <- 0 until numGlobalFiles) {
      withStream(getStream("GlobalModules"+i)) {
        close("}")
      }
    }

    withStream(getStream("GlobalRetiming")) {
      close("}")
    }

    if (Config.multifile >= 3 ) {
      val traits = streamMapReverse.keySet.toSet.map{
        f:String => f.split('.').dropRight(1).mkString(".")  /*strip extension */ 
      }.toSet - "AccelTop" - "Instantiator"

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
  val numArgIOs: Int,
  val loadStreamInfo: List[StreamParInfo],
  val storeStreamInfo: List[StreamParInfo],
  val streamInsInfo: List[StreamParInfo],
  val streamOutsInfo: List[StreamParInfo]
) extends ${traits.mkString("\n with ")} {

  // TODO: Figure out better way to pass constructor args to IOModule.  Currently just recreate args inside IOModule redundantly

}""")
      }
    } else {
    // Get traits that need to be mixed in
      val traits = streamMapReverse.keySet.toSet.map{
        f:String => f.split('.').dropRight(1).mkString(".")  /*strip extension */ 
      }.toSet - "AccelTop" - "Instantiator"
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
  val numArgIOs: Int,
  val loadStreamInfo: List[StreamParInfo],
  val storeStreamInfo: List[StreamParInfo],
  val numStreamIns: List[StreamParInfo],
  val numStreamOuts: List[StreamParInfo]
) extends ${traits.mkString("\n with ")} {

}
  // AccelTop class mixes in all the other traits and is instantiated by tester""")
      }
        
    }


    super.emitFileFooter()
  }

}
