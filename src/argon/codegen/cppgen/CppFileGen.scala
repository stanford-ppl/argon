package argon.codegen.cppgen

import argon.codegen.FileGen
import argon.Config

trait CppFileGen extends FileGen {
  import IR._


  override protected def emitMain[S:Staged](b: Block[S]): Unit = {
    emitBlock(b)
  }

  override protected def process[S:Staged](b: Block[S]): Block[S] = {

    // Forcefully create the following streams
    withStream(getStream("TopHost")) {
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Begin!")}
      preprocess(b)
      emitMain(b)
      postprocess(b)
      if (Config.emitDevel > 0) { Console.println(s"[ ${lang}gen ] Complete!")}
      b
    }
  }


  override protected def emitFileHeader() {
    emit(s"""#include "interface.h"
#include <stdint.h>
#include <sys/time.h>
#include <iostream>
#include <fstream>
#include <string> 
#include <sstream> 
#include <signal.h>
#include <sys/wait.h>
#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "DeliteCpp.h"
#include "cppDeliteArraystring.h"
#include "cppDeliteArrays.h"
#include "cppDeliteArraydouble.h"
#include "FringeContext.h"

void Top_run( Interface_t *args )
{
  // Constant fringe stuff

  // Create an execution context.
  FringeContext *c1 = new FringeContext("accel.bit.bin");

  // Program FPGA
  c1->load();

  // Set input arguments
  int numArgIns = sizeof(args->ArgIns)/sizeof(args->ArgIns[0]);
  for (int i=0; i<numArgIns; i++) {
    c1->setArg(i, (uint64_t)args->ArgIns[i]);
  }

  // Run FPGA
  c1->run();

  // Read output arguments
  int numArgOuts = sizeof(args->ArgOuts)/sizeof(args->ArgOuts[0]);
  for (int i = 0; i < numArgOuts; i++) {
    *args->ArgOuts[i] = c1->getArg(i);
  }

  // Cycles is the last argument out (I think)
  *args->ArgOuts[numArgOuts] = c1->getArg(numArgOuts);
}
""")

  open(s"void Application(int numThreads, cppDeliteArraystring * args) {")
  emit("Interface_t interface;")

    withStream(getStream("interface", "h")) {
      emit(s"""// Interface between delite c++ and hardware tester
  // class Interface_t {

  // public:
  //   int32_t* ArgIns[1]; // Can do sizeof this because we size arrays at codegen time
  //   int32_t* ArgOuts[1];
  //   long* MemIns[0];
  //   long* MemOuts[0][0];
  //   uint64_t* cycles;
  // };
  
#include <vector>
#include <iostream>
#include <stdint.h>

class Interface_t""")
      open("{")
      open(s"""public:""")
      emit("Interface_t()")
      emit("{}")
      emit("~Interface_t()")
      emit("{}")
    }

    withStream(getStream("cpptypes","h")) {
      emit("""#ifndef __CPPTYPES_H__
#define __CPPTYPES_H__
#include "DRAM.h" 
#endif""")
    }

    withStream(getStream("DRAM","h")){
      emit(s"""
#include <stdint.h>
#include <vector>
#include <iostream>

class DRAM {
public:
  uint64_t baseAddr;
  uint32_t size;

  DRAM(uint64_t base, int size) {
    this->baseAddr = base;
    this->size = size;
  }
  void add_mem(long num) { data.push_back(num); }
  long get_mem(int i) { return data[i]; }
  long data_length() { return data.size(); }

private:
  std::vector<long> data;

};""")
    }
    super.emitFileHeader()
  }

  override protected def emitFileFooter() {  
    close("}")
    emit(s"""
int main(int argc, char *argv[]) {
  cppDeliteArraystring *args = new cppDeliteArraystring(argc-1);
  for (int i=1; i<argc; i++) {
    args->update(i-1, *(new string(argv[i])));
  }
  int numThreads = 1;
  char *env_threads = getenv("DELITE_NUM_THREADS");
  if (env_threads != NULL) {
    numThreads = atoi(env_threads);
  } else {
    fprintf(stderr, "[WARNING]: DELITE_NUM_THREADS undefined, defaulting to 1\\n");
  }
  fprintf(stderr, "Executing with %d thread(s)\\n", numThreads);
  Application(numThreads, args);
  return 0;
}
""")

    withStream(getStream("interface", "h")) {
      emit(s"uint64_t* cycles;")
      emit(s"void add_mem(long num) { memOut.push_back(num);}")
      emit(s"long get_mem(int i) { return memOut[i]; }")
      emit(s"long memOut_length() { return memOut.size(); } // Cannot size in advance because multiple DRAMs will share this header")
      emit(s"private:")
      emit(s"std::vector<long> memOut;")
      close("")
      close("};")
    }

    super.emitFileFooter()
  }

}
