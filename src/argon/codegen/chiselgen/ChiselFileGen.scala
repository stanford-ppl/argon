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
import chisel3.util._
import fringe._
import types._""")
      open("trait IOModule extends Module {")
      emit("""val target = "" // TODO: Get this info from command line args (aws, de1, etc)""")
      emit("val io_w = 32 // TODO: How to generate these properly?")
      emit("val io_v = 16 // TODO: How to generate these properly?")
    }

    withStream(getStream("BufferControlCxns")) {
      emit(s"""package accel
import templates._
import fringe._
import chisel3._""")
      open(s"""trait BufferControlCxns extends RootController {""")
    }

    withStream(getStream("RootController")) {
      emit(s"""package accel
import templates._
import fringe._
import chisel3._""")
      open(s"trait RootController extends GlobalWires {")

    }

    withStream(getStream("GlobalWires")) {
      emit(s"""package accel
import templates._
import chisel3._
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

        emit("// ---- Fringe software API ----")
        open("def writeReg(reg: Int, data: Int) {")
          emit("poke(c.io.waddr, reg)")
          emit("poke(c.io.wdata, data)")
          emit("poke(c.io.wen, 1)")
          emit("step(1)")
          emit("poke(c.io.wen, 0)")
        close("}")

        open("def readReg(reg: Int): Int = {")
          emit("poke(c.io.raddr, reg)")
          emit("peek(c.io.rdata).toInt")
        close("}")

        open("def run() = {")
          emit("var numCycles = 0")
          emit("var status = 0")
          emit("writeReg(c.fringe.commandReg, 1)")
          open("while ((status == 0) && (numCycles <= 100)) {")
            emit("step(1)")
            emit("status = readReg(c.fringe.statusReg)")
            emit("numCycles += 1")
          close("}")
          emit("numCycles")
        close("}")

        emit("  // ---- Host code ----")
        emit("// Write to all argIns: Regs 2..numArgIns+2")
        open("for (i <- 2 until c.numArgIns+2) {")
          emit("if (i == 2) writeReg(i, 0x400)")
          emit("else writeReg(i, (i-1)*2)  // Write pattern 4,6..")
        close("}")
        emit("run()")
        emit("// Read all argOuts: numargIns+2..numArgIns+2+numArgOuts")
        open("val argOuts = List.tabulate(c.numArgOuts) { i =>")
          emit("readReg(c.numArgIns+2+i)")
        close("}")
        emit("")
        emit("""println(s"argOuts: $argOuts")""")
      close("}")
      emit("")
      open("object Instantiator extends CommonMain {")
        emit("type DUTType = Top")
        emit("")
        open("def supportedTarget(t: String) = t match {")
          emit("""case "aws" => true""")
          emit("""case "verilator" => true""")
          emit("case _ => false")
        close("}")
        emit("")
        open("def dut = () => {")

    }


    super.emitFileHeader()
  }

  override protected def emitFileFooter() {
    // emitBufferControlCxns()

    withStream(getStream("Instantiator")) {
          emit("val w = 32")
          emit("""val target = if (args.size > 0) args(0) else "verilator" """)
          emit("""Predef.assert(supportedTarget(target), s"ERROR: Unsupported Fringe target '$target'")""")
          emit("new Top(w, numArgIns, numArgOuts, numMemoryStreams, target)")
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
      open("val io = IO(new Bundle {")
        emit("// Control")
        emit("val enable = Input(Bool())")
        emit("val done = Output(Bool())")
        emit("")
        emit("// Tile Load")
        emit("val memStreams = Vec(io_numMemoryStreams, Flipped(new MemoryStream(io_w, io_v)))")
        emit("")
        emit("// Scalars")
        emit("val argIns = Input(Vec(io_numArgIns, UInt(io_w.W)))")
        emit("val argOuts = Vec(io_numArgOuts, Decoupled((UInt(io_w.W))))")
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

    if (Config.multifile == 4) {
      val traits = streamMapReverse.keySet.toSet.map{
        f:String => f.split('.').dropRight(1).mkString(".")  /*strip extension */ 
      }.toSet - "AccelTop" - "GlobalWires" - "Instantiator"

      withStream(getStream("AccelTop")) {
        emit(s"""package accel
import templates._
import fringe._
import chisel3._
import chisel3.util._
class AccelTop(val top_w: Int, val numArgIns: Int, val numArgOuts: Int, val numMemoryStreams: Int = 1) extends GlobalWires with ${(traits++Set("RootController")).mkString("\n with ")} {

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

class AccelTop(val top_w: Int, val numArgIns: Int, val numArgOuts: Int, val numMemoryStreams: Int = 1) extends GlobalWires with ${(traits++Set("RootController")).mkString("\n with ")} {

}
  // AccelTop class mixes in all the other traits and is instantiated by tester""")
      }
        
    }


    super.emitFileFooter()
  }

}
