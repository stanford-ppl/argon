package argon.codegen.cppgen

import argon._
import argon.codegen.FileGen

trait CppFileGen extends FileGen {

  override protected def emitMain[S:Type](b: Block[S]): Unit = emitBlock(b)

  override protected def process[S:Type](b: Block[S]): Block[S] = {
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
    emit(s"""#include <stdint.h>
#include <sys/time.h>
#include <iostream>
#include <fstream>
#include <string> 
#include <sstream> 
#include <stdarg.h>
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
#include "functions.h"
#include <vector>
using std::vector;

""")

  open(s"void Application(int numThreads, vector<string> * args) {")
  emit("// Create an execution context.")
  emit("""FringeContext *c1 = new FringeContext("./verilog/accel.bit.bin");""")
  emit("""c1->load();""")


    withStream(getStream("cpptypes","h")) {
      emit("""#ifndef __CPPTYPES_H__
#define __CPPTYPES_H__
#endif""")
    }

    withStream(getStream("functions","h")) {
      emit("""#ifndef __CPPFUN_H__
#define __CPPFUN_H__
""")
    }

    withStream(getStream("functions","cpp")) {
      emit("""#include "functions.h" """)
    }

//     withStream(getStream("DRAM","h")){
//       emit(s"""
// #include <stdint.h>
// #include <vector>
// #include <iostream>

// class DRAM {
// public:
//   uint64_t baseAddr;
//   uint32_t size;

//   DRAM(uint64_t base, int size) {
//     this->baseAddr = base;
//     this->size = size;
//   }
//   void add_mem(long num) { data.push_back(num); }
//   long get_mem(int i) { return data[i]; }
//   long data_length() { return data.size(); }

// private:
//   std::vector<long> data;

// };""")
//     }
    super.emitFileHeader()
  }

  override protected def emitFileFooter() {
    emit("delete c1;")
    close("}")
    emit(s"""
int main(int argc, char *argv[]) {
  vector<string> *args = new vector<string>(argc-1);
  for (int i=1; i<argc; i++) {
    (*args)[i-1] = std::string(argv[i]);
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

      withStream(getStream("functions","h")) {
        emit("""#endif""")
    }

    super.emitFileFooter()
  }

}
